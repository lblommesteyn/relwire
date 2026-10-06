// Pin-only harness, usable on the RTL or the gate-level netlist: programs
// tt_um_relwire through the loader (load.hex), applies wire biases (cfg.hex),
// runs for +cycles=, then stops and reads back each core's data memory.
// Writes pins.txt: every output pin on every cycle, then the readback bytes.
// Two runs (RTL, GL) agree iff the two files are identical.
`timescale 1ns / 1ps
module tb_pins;
  localparam NW = 4;
  reg clk = 0;
  always #10 clk = ~clk;  // 50 MHz
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
  integer cycles, nbytes, f, i, c, b;

  task send(input [7:0] v);
    begin
      ui_in = v;
      repeat (2) @(posedge clk);
      stb = 1;
      repeat (3) @(posedge clk);
      stb = 0;
      repeat (3) @(posedge clk);
    end
  endtask

  initial begin
    if (!$value$plusargs("dir=%s", dir)) dir = ".";
    if (!$value$plusargs("cycles=%d", cycles)) cycles = 1000;
    if (!$value$plusargs("bytes=%d", nbytes)) nbytes = 0;
    $readmemh({dir, "/cfg.hex"}, cfg);
    $readmemh({dir, "/load.hex"}, stream);
    f = $fopen({dir, "/pins.txt"}, "w");
    repeat (4) @(posedge clk);
    rst_n = 1;
    repeat (4) @(posedge clk);
    for (i = 0; i < nbytes; i = i + 1) send(stream[i]);
    for (i = 0; i < cycles; i = i + 1) begin
      @(negedge clk);
      $fwrite(f, "%h %h %h\n", uo_out, uio_oe, uio_out);
    end
    send(8'h07);
    for (c = 0; c < 4; c = c + 1)
      for (b = 0; b < 8; b = b + 1) begin
        send(8'h08);
        send(c);
        send(b);
        repeat (2) @(posedge clk);
        $fwrite(f, "rb %0d %0d %h\n", c, b, uo_out);
      end
    $fclose(f);
    $finish;
  end
endmodule
