`default_nettype none
`timescale 1ns / 1ps

// Testbench for the cocotb test: pull-ups on the four protocol wires (an
// open-drain I2C bus), and the loader strobe/frame bits driven by test.py.
module tb ();

  initial begin
    $dumpfile("tb.fst");
    $dumpvars(0, tb);
    #1;
  end

  reg clk;
  reg rst_n;
  reg ena;
  reg [7:0] ui_in;
  reg load_strobe;
  reg load_frame;
  wire [7:0] uo_out;
  wire [7:0] uio_out;
  wire [7:0] uio_oe;
  wire [3:0] bus;
  genvar i;
  generate
    for (i = 0; i < 4; i = i + 1) begin : pull
      assign bus[i] = uio_oe[i] ? uio_out[i] : 1'b1;
    end
  endgenerate
  wire [7:0] uio_in = {load_strobe, load_frame, 2'b00, bus};

`ifdef GL_TEST
  wire VPWR = 1'b1;
  wire VGND = 1'b0;
`endif

  tt_um_relwire user_project (
`ifdef GL_TEST
      .VPWR   (VPWR),
      .VGND   (VGND),
`endif
      .ui_in  (ui_in),
      .uo_out (uo_out),
      .uio_in (uio_in),
      .uio_out(uio_out),
      .uio_oe (uio_oe),
      .ena    (ena),
      .clk    (clk),
      .rst_n  (rst_n)
  );

endmodule
