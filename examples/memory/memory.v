module memory (clk, wr, re, addr ,din, dout);
	parameter DATA_WIDTH = 2;
	parameter ADDR_WIDTH = 2;
	parameter MEM_DEPTH = 1 << ADDR_WIDTH;
	
	input clk, wr, re;
	input [DATA_WIDTH-1:0] din;
	input [ADDR_WIDTH-1:0] addr;
	output[DATA_WIDTH-1:0] dout;
	
	reg [DATA_WIDTH-1:0] dout;
	reg [DATA_WIDTH-1:0] mem [0:MEM_DEPTH-1];
	
	always@ (posedge clk) 
	begin
		if (wr) begin
			mem[addr] = din;
		end
		dout = (!wr && re) ? (mem[addr]) : dout;
	end
endmodule