module sram(clk, addr, we, dataIn, dataOut);
	parameter DATA_WIDTH = 2;
	parameter ADDR_WIDTH = 2;
	parameter RAM_DEPTH = 1 << ADDR_WIDTH;
	
	input clk,we;
	input [1:0] addr;
	input [1:0] dataIn;
	output [1:0] dataOut;

	reg [1:0] dataOut;
	reg [1:0] mem [0:3];
	
	always @(posedge clk)
	begin
		dataOut = (!we) ? mem[addr] : dataOut;
		if(we)
			mem[addr] = dataIn;
	end
endmodule