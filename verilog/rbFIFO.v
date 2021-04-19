
module rbFIFO(clock,rst,dataIn,push,pop,dataOut,full,empty);
    parameter	    MSBD = 63;
    parameter	    LAST = 31;
    parameter	    MSBA = 4;
    input	    clock;
    input           rst;
    input [MSBD:0]  dataIn;
    input	    push;
    input	    pop;
    output [MSBD:0] dataOut;
    output	    full;
    output	    empty;

    reg [MSBD:0]    mem[0:LAST];
    reg [MSBA:0]    tail;
    reg [MSBA:0]    head;
    reg		    empty;
    reg		    full;
    integer	    i;

    initial begin
	for (i = 0; i <= LAST; i = i + 1)
	    mem[i] = 0;
	tail = 0;
    	head = 0;
	full = 0;
	empty = 1;
    end // initial begin

    always @ (posedge clock) begin
	if (rst) begin tail = 0;
			head = 0;
			full = 0;
	              empty = 1;
                 end
	else if (push & ~full) begin
	    mem[tail] = dataIn;
	    tail = tail + 1;
	    if (head == tail) full = 1;
	    empty = 0;
	end // if (push & ~full)
	else if (pop & ~empty) begin
	    head = head + 1;
	    if (tail == head)empty = 1;
	    full = 0;
	end // if (pop & ~empty)
    end // always @ (posedge clock)

    assign dataOut = mem[head];

endmodule // rbFIFO

