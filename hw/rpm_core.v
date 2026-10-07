// RelWire Protocol Machine core, ISA v1: executes one specialized
// (role-resolved) instruction stream. Instruction memory is external.
//
// Timebase: the protocol tick is K core cycles. Drives go to shadow
// registers and the harness commits them once per tick; [now]/[prev] are
// the bus values latched for this tick. Zero-time instructions chain within
// the tick's exec cycles (the reference model's "run until blocked").
//
// v1 vs v0: 26-bit instructions; timing values come from an 8-entry
// constant table shared by all cores (so a program's speed is data, not
// code), read through two ports per core; one level of zero-overhead loop
// with an index register that offsets data addresses; 64-bit data memory.
//
// One binary serves every role: an owned instruction carries its owner's
// role id in [25:24] and has bit 19 set; the core drives it iff [role_mask]
// has that role and the role has not been demoted by a lost arbitration.
//
// Encoding (26 bits): [25:24] role, [23:20] op, [19] owned, [18:17] wire,
// [16] lvl, [15:0]:
//   1 EDGE        [15:13] min k, [12:10] max k
//   2 PUT         [15] lit, [14] ix, [13:7] addr        (lvl = invert)
//   3 SAMPLE      [15] lit, [14] ix, [13:7] addr
//   4 AFTER       [15:13] k
//   5 TOGGLE      [14] ix, [13:7] addr, [2:0] k: nominal K[k], min K[k+1], max K[k+2]
//   6 BRANCH_RUN  [15:13] n, [12] set, [11:5] addr, [4:0] skip
//   7 BRANCH_BIT  [14] ix, [13:7] addr, [6:0] skip
//   8 JUMP        [9:0] skip
//   9 LOOP        [15:8] count, [7:0] offset of the body's last instruction
//   0 HALT
// A constant of 0xFFFF as a max means unbounded.

module rpm_core #(
    parameter NW = 4,
    parameter PCW = 8,
    parameter DW = 64
) (
    input wire clk,
    input wire rst,
    input wire clr,  // power-on clear of data memory (rst alone keeps it for the loader)
    input wire pre_en,  // predecode: the cycle before this core executes
    input wire exec_en,
    input wire [15:0] tick,
    input wire [NW-1:0] now,
    input wire [NW-1:0] prev,
    input wire [2*NW-1:0] res,  // per wire: 0 push-pull, 1 dominant-low, 2 dominant-high
    input wire [25:0] instr,
    input wire [3:0] role_mask,
    output reg [PCW-1:0] pc,
    output reg [NW-1:0] oe,
    output reg [NW-1:0] out,
    output reg ev_valid,
    output reg [2:0] ev_code,  // 1 match 2 peer 3 arb 4 deadline 5 early 6 collision 7 mismatch
    output reg [6:0] ev_addr,
    output reg halted,
    // two read ports into the shared constant table
    output wire [2:0] ksel_a,
    output wire [2:0] ksel_b,
    input wire [15:0] ka,
    input wire [15:0] kb,
    // byte write port into data memory (used by the loader while stopped)
    input wire ld_we,
    input wire [2:0] ld_byte,
    input wire [7:0] ld_data,
    output reg [DW-1:0] dmem
);
  localparam INF = 16'hFFFF;
  localparam READY = 2'd0, DRIVEN = 2'd1, AWAIT = 2'd2;

  reg [3:0] demoted;
  reg [1:0] phase;
  reg [15:0] anchor, t0;
  reg [2:0] run_len[0:NW-1];
  reg [NW-1:0] last;
  reg lactive;
  reg [PCW-1:0] lstart, lend;
  reg [7:0] lcount, idx;

  wire [3:0] op = instr[23:20];
  wire [1:0] role = instr[25:24];
  wire sup = instr[19] & role_mask[role] & ~demoted[role];
  wire [1:0] w = instr[18:17];
  wire lvl = instr[16];
  wire lit = instr[15];
  wire ix = instr[14];
  wire [6:0] addr = instr[13:7] + (ix ? idx[6:0] : 7'd0);

  // Port A: EDGE min, AFTER ticks, TOGGLE nominal (driving) or min (watching).
  // Port B: EDGE max, TOGGLE max.
  wire tg_watch = !(phase == READY && sup);
  assign ksel_a = op == 4'd5 ? instr[2:0] + {2'b0, tg_watch} : instr[15:13];
  assign ksel_b = op == 4'd5 ? instr[2:0] + 3'd2 : instr[12:10];
  wire [15:0] k_hi = ka, k_lo = kb;
  wire [15:0] tg_nom = ka, tg_min = ka, tg_max = kb;

  wire [15:0] d = tick - anchor;
  wire bus = now[w];
  wire rose = now[w] != prev[w];
  wire dbit = dmem[addr[5:0]];
  wire [1:0] rw = res[2*w+:2];

  // Predecode. Everything the execute cycle tests is stable for the whole
  // tick before it (the core's state only changes when it executes), so it is
  // computed one cycle early into registers: the execute cycle is then a short
  // next-state mux instead of decode, a 16-bit subtract and compares.
  reg [6:0] p_addr;
  reg p_bus, p_rose, p_dbit, p_ge_a, p_dl, p_run, p_last, p_tne;
  reg [1:0] p_rw;
  always @(posedge clk)
    if (pre_en) begin
      p_addr <= addr;
      p_bus <= bus;
      p_rose <= rose;
      p_dbit <= dbit;
      p_rw <= rw;
      p_ge_a <= d >= ka;                      // EDGE min, AFTER, TOGGLE nominal/min
      p_dl <= kb != INF && d > kb;            // EDGE max, TOGGLE max: deadline passed
      p_run <= run_len[w] >= instr[15:13];    // BRANCH_RUN
      p_last <= last[w];
      p_tne <= tick != t0;
    end

  function [1:0] drv(input [1:0] r, input v);
    case (r)
      2'd1: drv = v ? 2'b00 : 2'b10;
      2'd2: drv = v ? 2'b11 : 2'b00;
      default: drv = {1'b1, v};
    endcase
  endfunction

  integer i;

  // Every pc transfer goes through here: reaching the instruction after the
  // loop body (by falling through or by a skip) wraps to the body start.
  task goto(input [PCW-1:0] t);
    begin
      if (lactive && t == lend + 1'b1) begin
        if (idx + 1'b1 < lcount) begin
          pc <= lstart;
          idx <= idx + 1'b1;
        end else begin
          pc <= t;
          lactive <= 1'b0;
        end
      end else pc <= t;
    end
  endtask

  task finish;
    begin
      anchor <= tick;
      phase <= READY;
      goto(pc + 1'b1);
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
      p = drv(p_rw, v);
      oe[w] <= p[1];
      out[w] <= p[0];
    end
  endtask

  task lose;
    begin
      dmem[p_addr[5:0]] <= p_bus;
      emit(3'd3, p_addr);
      demoted[role] <= 1'b1;
      oe <= 0;
    end
  endtask

  always @(posedge clk) begin
    ev_valid <= 1'b0;
    if (clr) dmem <= 0;
    else if (ld_we) dmem[8*ld_byte+:8] <= ld_data;
    if (rst) begin
      pc <= 0;
      oe <= 0;
      out <= 0;
      halted <= 0;
      demoted <= 4'd0;
      phase <= READY;
      anchor <= 0;
      t0 <= 0;
      last <= 0;
      lactive <= 0;
      idx <= 0;
      for (i = 0; i < NW; i = i + 1) run_len[i] <= 0;
    end else if (exec_en && !halted) begin
      case (op)
        4'd0: begin  // HALT
          oe <= 0;
          halted <= 1'b1;
        end
        4'd2: begin  // PUT
          if (sup) drive(p_dbit ^ lvl);
          else oe[w] <= 1'b0;
          goto(pc + 1'b1);
        end
        4'd3: begin  // SAMPLE
          if (run_len[w] != 0 && last[w] == p_bus) begin
            if (run_len[w] != 3'd7) run_len[w] <= run_len[w] + 1'b1;
          end else run_len[w] <= 3'd1;
          last[w] <= p_bus;
          if (sup) begin
            if (p_dbit == p_bus) emit(3'd1, p_addr);
            else lose;
          end else if (lit && p_dbit != p_bus) emit(3'd7, p_addr);
          else dmem[p_addr[5:0]] <= p_bus;
          goto(pc + 1'b1);
        end
        4'd4: begin  // AFTER
          if (p_ge_a) begin
            anchor <= tick;
            goto(pc + 1'b1);
          end
        end
        4'd1, 4'd5: begin  // EDGE, TOGGLE
          if (phase == READY && sup) begin
            if (p_ge_a) begin
              drive(op == 4'd1 ? lvl : p_dbit);
              t0 <= tick;
              phase <= DRIVEN;
            end
          end else if (phase == DRIVEN) begin
            if (p_tne) begin
              if (op == 4'd1) begin
                if (p_bus == lvl) begin
                  emit(3'd1, 7'd0);
                  finish;
                end else begin
                  emit(3'd2, 7'd0);  // a peer holds the wire: the time is theirs
                  phase <= AWAIT;
                end
              end else begin
                // a data edge must actually transition: the right level with
                // no transition is a collision (the symbol never reached the wire)
                if (!p_rose) begin  // no transition: collision, adopt nothing
                  emit(3'd6, p_addr);
                  demoted[role] <= 1'b1;
                  oe <= 0;
                end else if (p_bus == p_dbit) emit(3'd1, p_addr);
                else lose;
                finish;
              end
            end
          end else if (op == 4'd1) begin  // observed / awaiting edge
            if (p_bus == lvl && p_rose) begin
              if (!p_ge_a) emit(3'd5, 7'd0);
              finish;
            end else if (p_dl) begin
              emit(3'd4, 7'd0);
              finish;
            end
          end else begin  // observed toggle: blanked before min, deadline max
            if (p_dl) begin
              emit(3'd4, 7'd0);
              finish;
            end else if (p_ge_a && p_rose) begin
              dmem[p_addr[5:0]] <= p_bus;
              finish;
            end
          end
        end
        4'd6: begin  // BRANCH_RUN
          if (p_run) begin
            if (instr[12]) dmem[instr[10:5]] <= ~p_last;
            goto(pc + 1'b1);
          end else goto(pc + 1'b1 + instr[4:0]);
        end
        4'd7: goto((p_dbit == lvl) ? pc + 1'b1 : pc + 1'b1 + instr[6:0]);  // BRANCH_BIT
        4'd8: goto(pc + 1'b1 + instr[9:0]);  // JUMP
        4'd9: begin  // LOOP
          lactive <= 1'b1;
          lstart <= pc + 1'b1;
          lend <= pc + instr[7:0];
          lcount <= instr[15:8];
          idx <= 0;
          pc <= pc + 1'b1;
        end
        default: halted <= 1'b1;
      endcase
    end
  end
endmodule
