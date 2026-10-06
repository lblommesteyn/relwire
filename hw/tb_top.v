// Pin-level harness for tt_um_relwire: programs the chip through the byte
// loader (load.hex), applies wire biases (cfg.hex) to the uio pins, runs,
// and records the bus per tick, every core's events, and data memory (the
// last also through the readback command, for core 0).
// Writes trace_hw.txt, events_hw.txt, dmem_hw.txt in +dir=.
`timescale 1ns / 1ps
module tb_top;
  localparam NC = 4, NW = 4;
  reg clk = 0;
  always #1 clk = ~clk;
  reg rst_n = 0;
  reg [7:0] ui_in = 0;
  reg stb = 0, frm = 0;
  wire [7:0] uo_out, uio_out, uio_oe;
  reg [7:0] cfg[0:NW-1];
  reg [NW-1:0] pins;
  integer w;
  always @* for (w = 0; w < NW; w = w + 1) pins[w] = uio_oe[w] ? uio_out[w] : cfg[w][4];

  tt_um_relwire dut (
      .ui_in(ui_in), .uo_out(uo_out), .uio_in({stb, frm, 2'b0, pins}), .uio_out(uio_out),
      .uio_oe(uio_oe), .ena(1'b1), .clk(clk), .rst_n(rst_n));

  reg [7:0] stream[0:16383];
  reg [1023:0] dir;
  integer ticks, nwires, nbytes, ft, fe, fd, i, c;

  task send(input [7:0] b);
    begin
      ui_in = b;
      repeat (2) @(posedge clk);
      stb = 1;
      repeat (3) @(posedge clk);
      stb = 0;
      repeat (3) @(posedge clk);
    end
  endtask

  // Event and trace capture: an event fired on the last slot of a tick is
  // seen after the tick counter has advanced.
  always @(posedge clk) begin
    #0.1;
    if (rst_n && dut.run) begin
      if (dut.slot == dut.CAPTURE && dut.tick < ticks) begin
        $fwrite(ft, "%0d ", dut.tick);
        for (w = 0; w < nwires; w = w + 1)
          if (!uio_oe[w] && cfg[w][5]) $fwrite(ft, "x");  // undriven, floating
          else $fwrite(ft, "%b", dut.now[w]);
        $fwrite(ft, "\n");
      end
      for (c = 0; c < NC; c = c + 1)
        if (dut.ev_valid[c])
          $fwrite(fe, "%0d %0d %0d %0d\n", c, dut.slot == 0 ? dut.tick - 1 : dut.tick,
                  dut.ev_code[c], dut.ev_addr[c]);
    end
  end

  reg [63:0] rb;
  initial begin
    if (!$value$plusargs("dir=%s", dir)) dir = ".";
    if (!$value$plusargs("ticks=%d", ticks)) ticks = 1000;
    if (!$value$plusargs("wires=%d", nwires)) nwires = NW;
    if (!$value$plusargs("bytes=%d", nbytes)) nbytes = 0;
    $readmemh({dir, "/cfg.hex"}, cfg);
    $readmemh({dir, "/load.hex"}, stream);
    ft = $fopen({dir, "/trace_hw.txt"}, "w");
    fe = $fopen({dir, "/events_hw.txt"}, "w");
    repeat (4) @(posedge clk);
    rst_n = 1;
    repeat (4) @(posedge clk);
    for (i = 0; i < nbytes; i = i + 1) send(stream[i]);  // ends with 06 (run)
    wait (dut.tick == ticks);
    @(posedge clk);
    #0.1;
    $fclose(ft);
    $fclose(fe);
    send(8'h07);
    // read core 0's data memory back through the pins
    for (i = 0; i < 8; i = i + 1) begin
      send(8'h08);
      send(8'h00);
      send(i);
      repeat (2) @(posedge clk);
      rb[8*i+:8] = uo_out;
    end
    fd = $fopen({dir, "/dmem_hw.txt"}, "w");
    for (c = 0; c < NC; c = c + 1) $fwrite(fd, "%h %0d\n", dut.dmem[c], dut.halted[c]);
    $fwrite(fd, "readback %h\n", rb);
    $fclose(fd);
    $finish;
  end
endmodule
