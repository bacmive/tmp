module sram(clk, addr, we, dataIn, dataOut);
	parameter DATA_WIDTH = 2;
	parameter ADDR_WIDTH = 2;
	parameter RAM_DEPTH = 1 << ADDR_WIDTH;
	
	input clk,we
	input [ADDR_WIDTH-1:0] addr;
	input [DATA_WIDTH-1:0] dataIn;
	
	reg [DATA_WIDTH-1;0] mem [0:RAM_DEPTH-1];
	
	always @(posedge clk)
	begin
		if(!we)
			mem[addr] = dataIn;
	end
 ​
	always @(posedge clk)begin
		if(we)
			dataOut = mem[addr];
	end
endmodule