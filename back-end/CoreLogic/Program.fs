open System
open Parser
open FParsec

let program = @"
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

  initial current_state[1] = 1;
  
  always @(posedge clk, negedge resetn)
  begin
    if(!resetn)
      begin
        current_state <= idle_st;
        b_reg <= 0;
        count_reg <= 0;
        data_reg <= 0;
        tx_reg <= 1'b1;
      end
    else
      begin
        current_state <= next_state;
        b_reg <= b_next;
        count_reg <= count_next;
        data_reg <= data_next;
        tx_reg <= tx_next;
      end
  end


  always @*
  begin
    next_state = current_state;
    tx_done = 1'b0;
    b_next = b_reg;
    count_next = count_reg;
    data_next = data_reg;
    tx_next = tx_reg;
    
    case(current_state)
      idle_st:
      begin
        tx_next = 1'b1;
        if(tx_start)
        begin
          next_state = start_st;
          b_next = 0;
          data_next = d_in;
        end
      end
      
      start_st: 
      begin
        tx_next = 1'b0;
        if(b_tick)
          if(b_reg==15)
            begin
              next_state = data_st;
              b_next = 0;
              count_next = 0;
            end
          else
            b_next = b_reg + 1;
      end
      
      data_st: 
      begin
        tx_next = data_reg[0];
        
        if(b_tick)
          if(b_reg == 15)
            begin
              b_next = 0;
              data_next = data_reg >> 1;
              if(count_reg == 8)    
                next_state = stop_st;
              else
                count_next = count_reg + 1;
            end
          else
            b_next = b_reg + 1;
      end
      
      stop_st: 
      begin
        tx_next = 1'b1;
        if(b_tick)
          if(b_reg == 15)  
            begin
              next_state = idle_st;
              tx_done = 1'b1;
            end
          else
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
  
  always @(posedge clk, negedge resetn)
  begin
    if(!resetn)
      begin
        current_state <= idle_st;
        b_reg <= 0;
        count_reg <= 0;
        data_reg <= 0;
        tx_reg <= 1'b1;
      end
    else
      begin
        current_state <= next_state;
        b_reg <= b_next;
        count_reg <= count_next;
        data_reg <= data_next;
        tx_reg <= tx_next;
      end
  end


  always @*
  begin
    next_state = current_state;
    tx_done = 1'b0;
    b_next = b_reg;
    count_next = count_reg;
    data_next = data_reg;
    tx_next = tx_reg;
    
    case(current_state)
      idle_st:
      begin
        tx_next = 1'b1;
        if(tx_start)
        begin
          next_state = start_st;
          b_next = 0;
          data_next = d_in;
        end
      end
      
      start_st: 
      begin
        tx_next = 1'b0;
        if(b_tick)
          if(b_reg==15)
            begin
              next_state = data_st;
              b_next = 0;
              count_next = 0;
            end
          else
            b_next = b_reg + 1;
      end
      
      data_st: 
      begin
        tx_next = data_reg[0];
        
        if(b_tick)
          if(b_reg == 15)
            begin
              b_next = 0;
              data_next = data_reg >> 1;
              if(count_reg == 8)    
                next_state = stop_st;
              else
                count_next = count_reg + 1;
            end
          else
            b_next = b_reg + 1;
      end
      
      stop_st: 
      begin
        tx_next = 1'b1;
        if(b_tick)
          if(b_reg == 15)  
            begin
              next_state = idle_st;
              tx_done = 1'b1;
            end
          else
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
  output reg [1:0] c;

  mod2 ayy(a,b,c[0]);

endmodule
"

let mod2 = @"
module mod2(a,b,c);

  input a;
  input b;
  output c;
  
  assign c = a + b;

endmodule
"

let print str = printfn "%A" str

[<EntryPoint>]
let main argv =
    let parser = LangConstructs.pSourceText
    // let result = 
    //     testStr
    //     |> run parser
    //     |> function
    //     | Success (ast,_,_) -> 
    //         let decs = Compiler.collectDecs [ast]
    //         printfn "****************"
    //         printfn "AST: \n%A" ast
    //         printfn "****************"
    //         Compiler.compileAST decs ast
    //     | Failure (msg,_,_) -> failwith msg
    // match result with
    // | Result.Error e -> printfn "%s" e
    // | Result.Ok net -> 
    //     printfn "%s" (net.ToString()) 
    [mod1; mod2]
    |> List.map (fun modStr ->
        modStr
        |> run parser
        |> function
        | Success (ast, _, _) -> ast
        | Failure (msg, _, _) -> failwith msg)
    |> Compiler.compileProject
    |> function
    | Result.Ok (topLevels, nets) ->
        printfn "Top Level Modules: %A" topLevels
        nets
        |> List.map (fun net -> printfn "%s" (net.ToString()))
        |> ignore
    | Result.Error e -> printfn "%s" e
    0 // return an integer exit code
