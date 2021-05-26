open System
open Parser
open FParsec
open CommonTypes

let program1 = @"
module UART_TX(
    input wire clk,
    input wire resetn,
    input wire tx_start,        
    input wire b_tick,          
    input wire [8:0] d_in,      
    output reg tx_done,         
    output wire tx           
    ); 
    
    reg [1:0] current_state;
    reg [1:0] next_state;
    reg [3:0] b_reg;          
    reg [3:0] b_next;
    reg [3:0] count_reg;      
    reg [3:0] count_next;
    reg [8:0] data_reg;       
    reg [8:0] data_next;
    reg tx_reg;              
    reg tx_next;
    
    always @(posedge clk, negedge resetn) begin
        if(!resetn) begin
            current_state <= 1;
            b_reg <= 0;
            count_reg <= 0;
            data_reg <= 0;
            tx_reg <= 1'b1;
        end else begin
            current_state <= next_state;
            b_reg <= b_next;
            count_reg <= count_next;
            data_reg <= data_next;
            tx_reg <= tx_next;
        end
    end

    always @* begin
        next_state = current_state;
        tx_done = 1'b0;
        b_next = b_reg;
        count_next = count_reg;
        data_next = data_reg;
        tx_next = tx_reg;

        case(current_state)
            1: begin
                tx_next = 1'b1;
                if(tx_start) begin
                    next_state = 2;
                    b_next = 0;
                    data_next = d_in;
                end
            end

            2: begin
                tx_next = 1'b0;
                if(b_tick)
                    if(b_reg==15)
                        begin
                            next_state = 3;
                            b_next = 0;
                            count_next = 0;
                        end
                    else
                        b_next = b_reg + 1;
            end

            3: begin
                tx_next = data_reg[0];

                if(b_tick)
                    if(b_reg == 15) begin
                        b_next = 0;
                        data_next = data_reg >> 1;
                        if(count_reg == 8)    
                            next_state = 4;
                        else
                            count_next = count_reg + 1;
                    end else
                        b_next = b_reg + 1;
            end

            4: begin
                tx_next = 1'b1;
                if(b_tick)
                    if(b_reg == 15) begin
                        next_state = 1;
                        tx_done = 1'b1;
                    end else
                        b_next = b_reg + 1;
            end
        endcase
    end
    
    assign tx = tx_reg;
    
endmodule
"

let program2 = @"
module UART_TX(
    clk,
    resetn,
    tx_start,        
    b_tick,          
    d_in,      
    tx_done,         
    tx              
    );
    input wire clk;
    input wire resetn;
    input wire tx_start;        
    input wire b_tick;          
    input wire [8:0] d_in;      
    output reg tx_done;         
    output wire tx;  
    
    reg [1:0] current_state;
    reg [1:0] next_state;
    reg [3:0] b_reg;          
    reg [3:0] b_next;
    reg [3:0] count_reg;      
    reg [3:0] count_next;
    reg [8:0] data_reg;       
    reg [8:0] data_next;
    reg tx_reg;              
    reg tx_next;
    
    always @(posedge clk, negedge resetn) begin
        if(!resetn) begin
            current_state <= 1;
            b_reg <= 0;
            count_reg <= 0;
            data_reg <= 0;
            tx_reg <= 1'b1;
        end else begin
            current_state <= next_state;
            b_reg <= b_next;
            count_reg <= count_next;
            data_reg <= data_next;
            tx_reg <= tx_next;
        end
    end

    always @* begin
        next_state = current_state;
        tx_done = 1'b0;
        b_next = b_reg;
        count_next = count_reg;
        data_next = data_reg;
        tx_next = tx_reg;

        case(current_state)
            1: begin
                tx_next = 1'b1;
                if(tx_start) begin
                    next_state = 2;
                    b_next = 0;
                    data_next = d_in;
                end
            end

            2: begin
                tx_next = 1'b0;
                if(b_tick)
                    if(b_reg==15)
                        begin
                            next_state = 3;
                            b_next = 0;
                            count_next = 0;
                        end
                    else
                        b_next = b_reg + 1;
            end

            3: begin
                tx_next = data_reg[0];

                if(b_tick)
                    if(b_reg == 15) begin
                        b_next = 0;
                        data_next = data_reg >> 1;
                        if(count_reg == 8)    
                            next_state = 4;
                        else
                            count_next = count_reg + 1;
                    end else
                        b_next = b_reg + 1;
            end

            4: begin
                tx_next = 1'b1;
                if(b_tick)
                    if(b_reg == 15) begin
                        next_state = 1;
                        tx_done = 1'b1;
                    end else
                        b_next = b_reg + 1;
            end
        endcase
    end
    
    assign tx = tx_reg;
    
endmodule
"

let mod1 = @"
module mod1(a,b,c);
    input a;
    input b;
    output [1:0] c;

    reg [1:0] d;

    assign {c[1], c[1]} = 5;

    initial begin
        d = 5;
    end

    mod2 yumm(a,b,c);
endmodule
"

let mod2 = @"
module mod2(a,b,c);
    input a;
    input b;
    output [1:0] c;
    
    assign c = a + b;
endmodule
"

let print str = printfn "%A" str

[<EntryPoint>]
let main _ =
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let startTiming a =
        stopwatch.Restart()
        a
    let stopTiming label a =
        stopwatch.Stop()
        printfn "[%s] elapsed (ms): %i" label stopwatch.ElapsedMilliseconds
        a

    let parser = LangConstructs.pSourceText
    [program1]
    |> List.map (fun modStr ->
        modStr
        |> startTiming
        |> run parser
        |> stopTiming "PAR"
        |> function
        | Success (ast, _, _) -> ast
        | Failure (msg, _, _) -> failwith msg)
    |> startTiming
    |> Compiler.Compile.project "UART_TX"
    |> stopTiming "COM"
    |> function
    | Compiler.CompResult.Fail _ as r -> printfn "%A" r
    | Compiler.CompResult.Succ s -> printfn "SUCC:\n %s" <| s.ToString()
    | Compiler.CompResult.Warn (s, w) ->
        printfn "WARN:\n %s" <| s.ToString()
        printfn "WARNINGS:\n %A" w
    0 // return an integer exit code
