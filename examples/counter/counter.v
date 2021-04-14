module counter ( clk, rst, dout);
	parameter LAST = 1;
	parameter MSBD = 1;
    
    input clk, rst;
    output [MSBD:0] dout;
    reg [1:0] last;
    
    always@ (posedge clk) begin
        if (rst)
            last = 2'b0;
        else begin
            last = last + 1;
        end
    end
	
	assign dout = last;
	
endmodule
