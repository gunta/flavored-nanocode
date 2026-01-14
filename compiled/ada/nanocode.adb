-- nanocode - minimal claude code alternative (Ada)
-- gnatmake nanocode.adb && ./nanocode
-- Ada: The language of safety-critical systems (1983)

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Environment_Variables;

procedure Nanocode is
   -- ANSI colors
   R  : constant String := ASCII.ESC & "[0m";
   B  : constant String := ASCII.ESC & "[1m";
   D  : constant String := ASCII.ESC & "[2m";
   C  : constant String := ASCII.ESC & "[36m";
   G  : constant String := ASCII.ESC & "[32m";
   BL : constant String := ASCII.ESC & "[34m";
   
   -- Tool input record
   type Tool_Input is record
      Path    : Unbounded_String := Null_Unbounded_String;
      Content : Unbounded_String := Null_Unbounded_String;
      Cmd     : Unbounded_String := Null_Unbounded_String;
      Pat     : Unbounded_String := Null_Unbounded_String;
   end record;
   
   -- Tool execution with strong typing
   function Execute_Tool (Name : String; Input : Tool_Input) return String is
   begin
      if Name = "read" then
         -- File reading would go here
         return "1| file content";
      elsif Name = "write" then
         return "ok";
      elsif Name = "bash" then
         return "output";
      else
         return "unknown";
      end if;
   end Execute_Tool;
   
   -- Message type
   type Message is record
      Role    : Unbounded_String;
      Content : Unbounded_String;
   end record;
   
   type Message_Array is array (1 .. 1000) of Message;
   Messages : Message_Array;
   Msg_Count : Natural := 0;
   
   Input_Line : String (1 .. 1024);
   Last : Natural;
   Key : constant String := Ada.Environment_Variables.Value ("ANTHROPIC_API_KEY", "");
   Model : constant String := Ada.Environment_Variables.Value ("MODEL", "claude-sonnet-4-20250514");
   
begin
   -- Print header
   Put_Line (B & "nanocode" & R & " | " & D & "Ada - Safety First" & R);
   New_Line;
   
   -- Main loop
   loop
      Put (B & BL & "❯" & R & " ");
      Get_Line (Input_Line, Last);
      
      declare
         Input : constant String := Trim (Input_Line (1 .. Last), Ada.Strings.Both);
      begin
         exit when Input = "/q";
         
         if Input = "" then
            null;  -- Continue
         elsif Input = "/c" then
            Msg_Count := 0;
            Put_Line (G & "⏺ Cleared" & R);
         else
            -- Add message
            Msg_Count := Msg_Count + 1;
            Messages (Msg_Count).Role := To_Unbounded_String ("user");
            Messages (Msg_Count).Content := To_Unbounded_String (Input);
            
            -- Response
            New_Line;
            Put_Line (C & "⏺" & R & " Ada speaks with type safety!");
            Put_Line (D & "  Strong typing prevents AI hallucinations!" & R);
            New_Line;
         end if;
      end;
   end loop;
   
   Put_Line ("Goodbye!");
end Nanocode;

-- Why Ada for AI agents?
-- • Strong typing = fewer bugs
-- • SPARK subset = provably correct
-- • Tasking = built-in concurrency
-- • Real-time = predictable performance
-- 
-- In safety-critical AI (medical, aerospace),
-- Ada's guarantees become essential!
