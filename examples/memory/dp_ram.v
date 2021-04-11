//dual port ram
module dp_ram (clk, 
			   wea, rea, addra, dina, douta
			   web, reb, addrb, dinb, doutb );
	parameter DATA_WIDTH = 2;
	parameter ADDR_WIDTH = 2;
	parameter RAM_DEPTH = 1 << ADDR_WIDTH;
	
	input clk, wea, web, rea, reb;
	input [ADDR_WIDTH-1:0] addra;
	input [DATA_WIDTH-1:0] dina;
	input [ADDR_WIDTH-1:0] addrb;
	input [DATA_WIDTH-1:0] dinb;
	output [DATA_WIDTH-1:0] douta;
	output [DATA_WIDTH-1:0] doutb;
	
	reg [DATA_WIDTH-1:0] douta;
	reg [DATA_WIDTH-1:0] doutb;
	reg [DATA_WIDTH] mem [0:RAM_DEPTH-1];
	
	always @(posedge clk)begin
        if(!wea)begin
			mem[addra] = dina;
			douta = dina;
		end 
		else begin
			douta = mem[addra];    
		end
	end
	 ​
	always @(posedge clk)begin
		if(!web)begin
			mem[addrb] = dinb;
			doutb = dinb;
		end
		else begin
			doutb = mem[addrb];
		end
	 end
endmodule