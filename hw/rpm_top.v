// RelWire protocol emulator, Tiny Tapeout top level.
//
// NC cores share one single-port program memory (128 x 26 flip-flops,
// registered read) and one 8-entry constant table (the timing). The memory is
// plain logic rather than an IHP SRAM macro: on CMOS5L the macro's Metal4
// power pins cannot reach Tiny Tapeout's Metal4-only power grid without
// TopMetal1, which user designs may not use. Every core runs the same binary; a 4-bit
// role mask per core says which roles it plays.
//
// Protocol wires: uio[3:0]. Loader: byte on ui_in, strobe on uio_in[7]
// (rising edge), frame reset on uio_in[6]. Status / readback on uo_out.
//
// Tick schedule (T = 3 + SYNC + EXEC*NC cycles): slot 0 commits the cores'
// drives to the pins; the pins pass a SYNC-flop synchronizer (external
// devices are asynchronous) and are latched as this tick's bus value at slot
// 1 + SYNC; then EXEC rounds of NC exec slots. The SRAM is read one slot
// ahead of the core that executes, so each core gets EXEC instructions per
// tick. EXEC = 1 is single issue: every instruction costs one tick, which
// the timing certificate accounts for (certify ~issue:1).
//
// Loader commands (first byte), arguments follow:
//   01 addr b3 b2 b1 b0   program word (low 26 bits)
//   02 i hi lo            timing constant i
//   03 core byte data     data-memory byte
//   04 core mask          role mask
//   05 wire res           wire resolution (0 push-pull, 1 dom-low, 2 dom-high)
//   06                    run       07   stop
//   08 core byte          read back a data-memory byte on uo_out

module tt_um_relwire #(
    parameter NC = 4,
    parameter EXEC = 1,
    parameter SYNC = 2
) (
    input wire [7:0] ui_in,
    output wire [7:0] uo_out,
    input wire [7:0] uio_in,
    output wire [7:0] uio_out,
    output wire [7:0] uio_oe,
    input wire ena,
    input wire clk,
    input wire rst_n
);
  localparam NW = 4;
  localparam T = 3 + SYNC + EXEC * NC;
  localparam LATCH = 1 + SYNC;  // slot at which the bus value is latched
  localparam CAPTURE = LATCH + 1;  // first slot at which [now] holds it
  localparam CB = $clog2(NC);

  // ---------------- loader ----------------
  reg [2:0] stb_s;
  reg [1:0] frm_s;
  always @(posedge clk) begin
    stb_s <= {stb_s[1:0], uio_in[7]};
    frm_s <= {frm_s[0], uio_in[6]};
  end
  wire byte_in = stb_s[1] & ~stb_s[2];

  reg [3:0] cnt;
  reg [7:0] cmd, a0, a1, a2, a3, a4;
  reg run;
  reg [3:0] masks[0:NC-1];
  reg [1:0] wres[0:NW-1];
  reg [15:0] kt[0:7];
  reg rb_mode;
  reg [CB-1:0] rb_core;
  reg [2:0] rb_byte;

  // SRAM write request from the loader
  reg sram_we;
  reg [7:0] sram_waddr;
  reg [25:0] sram_wdata;
  // data-memory byte write to one core
  reg ld_we;
  reg [CB-1:0] ld_core;
  reg [2:0] ld_byte;
  reg [7:0] ld_data;

  integer i;
  always @(posedge clk) begin
    sram_we <= 1'b0;
    ld_we <= 1'b0;
    if (!rst_n) begin
      cnt <= 0;
      run <= 0;
      rb_mode <= 0;
      for (i = 0; i < NC; i = i + 1) masks[i] <= 0;
      for (i = 0; i < NW; i = i + 1) wres[i] <= 0;
    end else if (frm_s[1]) cnt <= 0;
    else if (byte_in) begin
      cnt <= cnt + 1'b1;
      case (cnt)
        0: cmd <= ui_in;
        1: a0 <= ui_in;
        2: a1 <= ui_in;
        3: a2 <= ui_in;
        4: a3 <= ui_in;
        default: a4 <= ui_in;
      endcase
      // a command completes on its last byte; then the counter restarts
      if (cnt == 0 && (ui_in == 8'h06 || ui_in == 8'h07)) begin
        run <= ui_in == 8'h06;
        rb_mode <= 1'b0;
        cnt <= 0;
      end
      if (cnt == 5 && cmd == 8'h01) begin
        sram_we <= 1'b1;
        sram_waddr <= a0;
        sram_wdata <= {a1[1:0], a2, a3, ui_in};
        cnt <= 0;
      end
      if (cnt == 3 && cmd == 8'h02) begin
        kt[a0[2:0]] <= {a1, ui_in};
        cnt <= 0;
      end
      if (cnt == 3 && cmd == 8'h03) begin
        ld_we <= 1'b1;
        ld_core <= a0[CB-1:0];
        ld_byte <= a1[2:0];
        ld_data <= ui_in;
        cnt <= 0;
      end
      if (cnt == 2 && cmd == 8'h04) begin
        masks[a0[CB-1:0]] <= ui_in[3:0];
        cnt <= 0;
      end
      if (cnt == 2 && cmd == 8'h05) begin
        wres[a0[1:0]] <= ui_in[1:0];
        cnt <= 0;
      end
      if (cnt == 2 && cmd == 8'h08) begin
        rb_mode <= 1'b1;
        rb_core <= a0[CB-1:0];
        rb_byte <= ui_in[2:0];
        cnt <= 0;
      end
    end
  end

  // ---------------- tick schedule ----------------
  reg [$clog2(T)-1:0] slot;
  reg [15:0] tick;
  reg [NW-1:0] now, prev;
  reg [NW-1:0] pin_oe, pin_out;
  wire cores_rst = !rst_n || !run;
  reg [NW-1:0] sync_r[0:SYNC-1];
  integer si;
  always @(posedge clk) begin
    sync_r[0] <= uio_in[NW-1:0];
    for (si = 1; si < SYNC; si = si + 1) sync_r[si] <= sync_r[si-1];
  end
  wire [NW-1:0] pins_in = sync_r[SYNC-1];

  always @(posedge clk) begin
    if (cores_rst) begin
      slot <= 0;
      tick <= 0;
    end else begin
      slot <= (slot == T - 1) ? 0 : slot + 1'b1;
      if (slot == T - 1) tick <= tick + 1'b1;
      if (slot == LATCH) begin
        prev <= (tick == 0) ? pins_in : now;
        now <= pins_in;
      end
    end
  end

  wire fetching = slot >= LATCH + 1 && slot < LATCH + 1 + EXEC * NC;
  wire executing = slot >= LATCH + 2;
  wire [CB-1:0] fetch_core = (slot - (LATCH + 1)) % NC;
  wire [CB-1:0] exec_core = (slot - (LATCH + 2)) % NC;

  // ---------------- cores ----------------
  wire [7:0] pc[0:NC-1];
  wire [NW-1:0] oe[0:NC-1], out[0:NC-1];
  wire [2:0] ksa[0:NC-1], ksb[0:NC-1];
  wire ev_valid[0:NC-1];
  wire [2:0] ev_code[0:NC-1];
  wire [6:0] ev_addr[0:NC-1];
  wire halted[0:NC-1];
  wire [63:0] dmem[0:NC-1];
  wire [25:0] instr;
  wire [2*NW-1:0] res = {wres[3], wres[2], wres[1], wres[0]};
  wire [15:0] ka = kt[ksa[exec_core]], kb = kt[ksb[exec_core]];

  genvar g;
  generate
    for (g = 0; g < NC; g = g + 1) begin : core
      rpm_core #(.NW(NW)) u (
          .clk(clk), .rst(cores_rst), .clr(!rst_n), .exec_en(executing && exec_core == g),
          .tick(tick), .now(now), .prev(prev), .res(res), .instr(instr),
          .role_mask(masks[g]), .pc(pc[g]), .oe(oe[g]), .out(out[g]),
          .ev_valid(ev_valid[g]), .ev_code(ev_code[g]), .ev_addr(ev_addr[g]),
          .halted(halted[g]), .ksel_a(ksa[g]), .ksel_b(ksb[g]), .ka(ka), .kb(kb),
          .ld_we(ld_we && ld_core == g), .ld_byte(ld_byte), .ld_data(ld_data),
          .dmem(dmem[g]));
    end
  endgenerate

  // ---------------- program memory ----------------
  // 128 words; registered read one slot ahead of execution, as an SRAM would.
  localparam DEPTH = 128;
  reg [25:0] prog_mem[0:DEPTH-1];
  reg [25:0] dout;
  assign instr = dout;
  always @(posedge clk) begin
    if (sram_we) prog_mem[sram_waddr[6:0]] <= sram_wdata;
    else if (fetching) dout <= prog_mem[pc[fetch_core][6:0]];
  end

  // ---------------- pins ----------------
  // one loop variable per always block: a shared one would have two drivers
  reg [NW-1:0] any0, any1, anyoe;
  integer ca, wp, cs;
  always @* begin
    any0 = 0;
    any1 = 0;
    anyoe = 0;
    for (ca = 0; ca < NC; ca = ca + 1) begin
      any0 = any0 | (oe[ca] & ~out[ca]);
      any1 = any1 | (oe[ca] & out[ca]);
      anyoe = anyoe | oe[ca];
    end
  end

  always @(posedge clk) begin
    if (cores_rst) begin
      pin_oe <= 0;
      pin_out <= 0;
    end else if (slot == 0) begin
      for (wp = 0; wp < NW; wp = wp + 1)
        case (wres[wp])
          2'd1: begin pin_oe[wp] <= any0[wp]; pin_out[wp] <= 1'b0; end
          2'd2: begin pin_oe[wp] <= any1[wp]; pin_out[wp] <= 1'b1; end
          default: begin pin_oe[wp] <= anyoe[wp]; pin_out[wp] <= any1[wp]; end
        endcase
    end
  end

  assign uio_oe = {4'b0, pin_oe};
  assign uio_out = {4'b0, pin_out};

  // ---------------- status ----------------
  reg [2:0] last_code;
  reg [CB-1:0] last_core;
  always @(posedge clk)
    if (!rst_n) begin
      last_code <= 0;
      last_core <= 0;
    end else
      for (cs = 0; cs < NC; cs = cs + 1)
        if (ev_valid[cs]) begin
          last_code <= ev_code[cs];
          last_core <= cs[CB-1:0];
        end

  wire all_halted = halted[0] & halted[1] & (NC < 3 || halted[2]) & (NC < 4 || halted[3]);
  assign uo_out = rb_mode ? dmem[rb_core][8*rb_byte+:8]
                          : {run, all_halted, last_code, last_core[1:0], 1'b0};

  wire _unused = &{ena, 1'b0};
endmodule
