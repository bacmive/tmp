module counter ( clk, rst, dout);
	parameter LAST = 1;
	parameter MSBD = 2;

    input clk, rst;
    output [MSBD-1:0] dout;

    reg [MSBD-1:0] last;
    
    always @ (posedge clk) begin
        if (rst)
            last = 0;
        else begin
            last = last + 1;
        end
    end

    assign dout = last;
endmodule
