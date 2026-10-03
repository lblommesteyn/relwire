// RelWire Protocol Machine core, ISA v0: executes one specialized
// (role-resolved) instruction stream. Instruction memory is external.
//
// Timebase: the protocol tick is K core cycles. Drives go to shadow
// registers and the harness commits them once per tick; [now]/[prev] are
// the bus values latched for this tick. Zero-time instructions (put,
// sample, branch, jump, a satisfied after/edge) chain within the tick's
// exec cycles, which is how the reference model's "run until blocked"
// maps to hardware.
//
// Encoding (64 bits):
//   [63:60] op   0 HALT 1 EDGE 2 PUT 3 SAMPLE 4 AFTER 5 TOGGLE
//                6 BRANCH_RUN 7 BRANCH_BIT 8 JUMP
//   [59] sup     this role owns the variable (statically specialized)
//   [58:57] wire
//   [56] lvl     EDGE/BRANCH_BIT level, PUT invert
//   [55] lit     SAMPLE: checked constant; BRANCH_RUN: writes [addr]
//   [54:48] addr data-memory bit
//   [47:32] a    EDGE min, AFTER ticks, TOGGLE nominal, BRANCH_RUN n
//   [31:16] b    EDGE max, TOGGLE min, BRANCH/JUMP skip
//   [15:0]  c    TOGGLE max
//   0xFFFF as a max means unbounded.

module rpm_core #(
    parameter NW = 4,     // wires
    parameter PCW = 10,
    parameter DW = 128    // data-memory bits
) (
    input wire clk,
    input wire rst,
    input wire exec_en,
    input wire [15:0] tick,
    input wire [NW-1:0] now,
    input wire [NW-1:0] prev,
    input wire [2*NW-1:0] res,  // per wire: 0 push-pull, 1 dominant-low, 2 dominant-high
    input wire [63:0] instr,
    output reg [PCW-1:0] pc,
    output reg [NW-1:0] oe,
    output reg [NW-1:0] out,
    output reg ev_valid,
    output reg [2:0] ev_code,  // 1 match 2 peer 3 arb 4 deadline 5 early 7 mismatch
    output reg [6:0] ev_addr,
    output reg halted,
    // data memory load/inspect port
    input wire dmem_we,
    input wire [DW-1:0] dmem_wdata,
    output reg [DW-1:0] dmem
);
  localparam INF = 16'hFFFF;
  localparam READY = 2'd0, DRIVEN = 2'd1, AWAIT = 2'd2;

  reg demoted;
  wire [3:0] op = instr[63:60];
  wire sup = instr[59] & ~demoted;
  wire [1:0] w = instr[58:57];
  wire lvl = instr[56];
  wire lit = instr[55];
  wire [6:0] addr = instr[54:48];
  wire [15:0] a = instr[47:32];
  wire [15:0] b = instr[31:16];
  wire [15:0] c = instr[15:0];

  reg [1:0] phase;
  reg [15:0] anchor, t0;
  reg [2:0] run_len[0:NW-1];
  reg [NW-1:0] last;

  wire [15:0] d = tick - anchor;
  wire bus = now[w];
  wire rose = now[w] != prev[w];
  wire dbit = dmem[addr];
  wire [1:0] rw = res[2*w+:2];

  // {oe, out} that puts logical level [v] on a wire of resolution [r]
  function [1:0] drv(input [1:0] r, input v);
    case (r)
      2'd1: drv = v ? 2'b00 : 2'b10;
      2'd2: drv = v ? 2'b11 : 2'b00;
      default: drv = {1'b1, v};
    endcase
  endfunction

  integer i;

  task finish;
    begin
      anchor <= tick;
      phase <= READY;
      pc <= pc + 1'b1;
    end
  endtask

  task emit(input [2:0] code, input [6:0] ad);
    begin
      ev_valid <= 1'b1;
      ev_code <= code;
      ev_addr <= ad;
    end
  endtask

  task drive(input v);
    reg [1:0] p;
    begin
      p = drv(rw, v);
      oe[w] <= p[1];
      out[w] <= p[0];
    end
  endtask

  always @(posedge clk) begin
    ev_valid <= 1'b0;
    if (rst) begin
      pc <= 0;
      oe <= 0;
      out <= 0;
      halted <= 0;
      demoted <= 0;
      phase <= READY;
      anchor <= 0;
      t0 <= 0;
      last <= 0;
      for (i = 0; i < NW; i = i + 1) run_len[i] <= 0;
      if (dmem_we) dmem <= dmem_wdata;
    end else if (exec_en && !halted) begin
      case (op)
        4'd0: begin  // HALT
          oe <= 0;
          halted <= 1'b1;
        end
        4'd2: begin  // PUT
          if (sup) drive(dbit ^ lvl);
          else oe[w] <= 1'b0;
          pc <= pc + 1'b1;
        end
        4'd3: begin  // SAMPLE
          if (run_len[w] != 0 && last[w] == bus) begin
            if (run_len[w] != 3'd7) run_len[w] <= run_len[w] + 1'b1;
          end else run_len[w] <= 3'd1;
          last[w] <= bus;
          if (sup) begin
            if (dbit == bus) emit(3'd1, addr);
            else begin
              dmem[addr] <= bus;
              emit(3'd3, addr);
              demoted <= 1'b1;
              oe <= 0;
            end
          end else if (lit && dbit != bus) emit(3'd7, addr);
          else dmem[addr] <= bus;
          pc <= pc + 1'b1;
        end
        4'd4: begin  // AFTER
          if (d >= a) begin
            anchor <= tick;
            pc <= pc + 1'b1;
          end
        end
        4'd1, 4'd5: begin  // EDGE, TOGGLE
          if (phase == READY && sup) begin
            if (d >= a) begin
              drive(op == 4'd1 ? lvl : dbit);
              t0 <= tick;
              phase <= DRIVEN;
            end
          end else if (phase == DRIVEN) begin
            if (tick != t0) begin
              if (op == 4'd1) begin
                if (bus == lvl) begin
                  emit(3'd1, 7'd0);
                  finish;
                end else begin
                  emit(3'd2, 7'd0);  // a peer holds the wire: the time is theirs
                  phase <= AWAIT;
                end
              end else begin
                if (bus == dbit) emit(3'd1, addr);
                else begin
                  dmem[addr] <= bus;
                  emit(3'd3, addr);
                  demoted <= 1'b1;
                  oe <= 0;
                end
                finish;
              end
            end
          end else if (op == 4'd1) begin  // observed / awaiting edge
            if (bus == lvl && rose) begin
              if (d < a) emit(3'd5, 7'd0);
              finish;
            end else if (b != INF && d > b) begin
              emit(3'd4, 7'd0);
              finish;
            end
          end else begin  // observed toggle: blanked before b, deadline c
            if (c != INF && d > c) begin
              emit(3'd4, 7'd0);
              finish;
            end else if (d >= b && rose) begin
              dmem[addr] <= bus;
              finish;
            end
          end
        end
        4'd6: begin  // BRANCH_RUN
          if (run_len[w] >= a[2:0] && a <= 16'd7) begin
            if (lit) dmem[addr] <= ~last[w];
            pc <= pc + 1'b1;
          end else pc <= pc + 1'b1 + b[PCW-1:0];
        end
        4'd7: pc <= (dbit == lvl) ? pc + 1'b1 : pc + 1'b1 + b[PCW-1:0];  // BRANCH_BIT
        4'd8: pc <= pc + 1'b1 + b[PCW-1:0];  // JUMP
        default: halted <= 1'b1;
      endcase
    end
  end
endmodule
