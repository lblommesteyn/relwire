// Differential-test harness: up to 4 rpm_cores on a shared bus of 4 wires.
// Files in +dir=: cfg.hex (per wire: res, bias), prog.hex (shared), roles.hex,
// data.hex, consts.hex.
// Writes trace.txt (bus per tick), events.txt, dmem.txt.
`timescale 1ns / 1ps
module tb;
  localparam NC = 4, NW = 4, K = 6;  // cycle 0 commit, 1 latch, 2..5 exec
  reg clk = 0;
  always #1 clk = ~clk;

  reg rst = 1;
  reg [2:0] ph = 0;
  reg [15:0] tick = 0;
  reg [NW-1:0] now = 0, prev = 0;
  reg [25:0] imem[0:255];  // one program for every core
  reg [3:0] masks[0:NC-1];

  reg [63:0] dinit[0:NC-1];
  reg [7:0] cfg[0:NW-1];  // {bias[3:0], res[3:0]}; bias 0 low 1 high 2 float
  wire [2*NW-1:0] res;
  genvar g;
  generate
    for (g = 0; g < NW; g = g + 1) begin : rescfg
      assign res[2*g+:2] = cfg[g][1:0];
    end
  endgenerate

  wire [NW-1:0] oe[0:NC-1], out[0:NC-1];
  reg [NW-1:0] poe[0:NC-1], pout[0:NC-1];
  wire [7:0] pc[0:NC-1];
  wire ev_valid[0:NC-1];
  wire [2:0] ev_code[0:NC-1];
  wire [6:0] ev_addr[0:NC-1];
  wire halted[0:NC-1];
  wire [63:0] dmem[0:NC-1];
  reg [15:0] kt[0:7];  // the shared constant table
  wire [2:0] ksa[0:NC-1], ksb[0:NC-1];

  generate
    for (g = 0; g < NC; g = g + 1) begin : core
      rpm_core #(.NW(NW)) u (
          .clk(clk), .rst(rst), .exec_en(ph >= 2), .tick(tick), .now(now), .prev(prev),
          .res(res),
          .instr(imem[pc[g]]), .role_mask(masks[g]), .pc(pc[g]), .oe(oe[g]), .out(out[g]),
          .ev_valid(ev_valid[g]), .ev_code(ev_code[g]), .ev_addr(ev_addr[g]),
          .halted(halted[g]), .ksel_a(ksa[g]), .ksel_b(ksb[g]), .ka(kt[ksa[g]]),
          .kb(kt[ksb[g]]), .load(rst), .dmem_wdata(dinit[g]), .dmem(dmem[g]));
    end
  endgenerate

  // Bus resolution from committed drives: 2-state, x on contention/float.
  reg [NW-1:0] bus_v, bus_x;
  integer c, wi;
  always @* begin
    for (wi = 0; wi < NW; wi = wi + 1) begin : res_loop
      reg h0, h1;
      h0 = 0;
      h1 = 0;
      for (c = 0; c < NC; c = c + 1) begin
        if (poe[c][wi] && !pout[c][wi]) h0 = 1;
        if (poe[c][wi] && pout[c][wi]) h1 = 1;
      end
      bus_x[wi] = 0;
      case (cfg[wi][1:0])
        2'd1: bus_v[wi] = h0 ? 0 : h1 ? 1 : cfg[wi][4];
        2'd2: bus_v[wi] = h1 ? 1 : h0 ? 0 : cfg[wi][4];
        default: begin
          bus_v[wi] = h1;
          bus_x[wi] = h0 && h1;
          if (!h0 && !h1) bus_v[wi] = cfg[wi][4];
        end
      endcase
      if (!h0 && !h1 && cfg[wi][5]) bus_x[wi] = 1;  // floating
    end
  end

  reg [1023:0] dir;
  integer ticks, nwires, ft, fe, fd, i;
  initial begin
    if (!$value$plusargs("dir=%s", dir)) dir = ".";
    if (!$value$plusargs("ticks=%d", ticks)) ticks = 1000;
    if (!$value$plusargs("wires=%d", nwires)) nwires = NW;
    $readmemh({dir, "/cfg.hex"}, cfg);
    $readmemh({dir, "/prog.hex"}, imem);
    $readmemh({dir, "/roles.hex"}, masks);
    $readmemh({dir, "/data.hex"}, dinit);
    $readmemh({dir, "/consts.hex"}, kt);
    for (c = 0; c < NC; c = c + 1) begin
      poe[c] = 0;
      pout[c] = 0;
    end
    ft = $fopen({dir, "/trace_hw.txt"}, "w");
    fe = $fopen({dir, "/events_hw.txt"}, "w");
    @(posedge clk);
    @(posedge clk);
    rst = 0;
    while (tick < ticks) begin
      // cycle 0: commit drives from the previous tick
      ph = 0;
      for (c = 0; c < NC; c = c + 1) begin
        poe[c] = oe[c];
        pout[c] = out[c];
      end
      @(posedge clk);
      #0.1;
      // cycle 1: latch this tick's bus
      ph = 1;
      prev = (tick == 0) ? bus_v : now;
      now = bus_v;
      $fwrite(ft, "%0d ", tick);
      for (wi = 0; wi < nwires; wi = wi + 1) $fwrite(ft, "%s", bus_x[wi] ? "x" : (bus_v[wi] ? "1" : "0"));
      $fwrite(ft, "\n");
      @(posedge clk);
      #0.1;
      for (i = 2; i < K; i = i + 1) begin
        ph = i;
        @(posedge clk);
        #0.1;
        for (c = 0; c < NC; c = c + 1)
          if (ev_valid[c]) $fwrite(fe, "%0d %0d %0d %0d\n", c, tick, ev_code[c], ev_addr[c]);
      end
      tick = tick + 1;
    end
    fd = $fopen({dir, "/dmem_hw.txt"}, "w");
    for (c = 0; c < NC; c = c + 1) $fwrite(fd, "%h %0d\n", dmem[c], halted[c]);
    $fclose(ft);
    $fclose(fe);
    $fclose(fd);
    $finish;
  end
endmodule
