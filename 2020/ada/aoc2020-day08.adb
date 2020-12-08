with Text_IO; use Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
package body AOC2020.Day08 is
   type Operation is (Nop, Jmp, Acc);
   type Instruction is record
       Op : Operation;
       Value : Integer;
   end record;
   
   package Instruction_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Instruction);
   use Instruction_Vectors;
   
   package Boolean_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Natural);
   use Boolean_Sets;
   procedure Parse_Line (Line : Unbounded_String; Inst : out Instruction) is
      Op : Operation;
      Value : Integer;
   begin
      Op := Operation'Value(Slice(Line, 1, 3));
      Value := Integer'Value(Slice(Line, 5, Length(Line)));
      Inst := (Op => Op, Value => Value);
   end Parse_Line;
   procedure Read_File (IV : out Vector) is
      Fin : File_Type;
   begin
      Open (Fin, In_File, "../input/08.txt");
      while not End_Of_File(Fin) loop
         declare
            Line : Unbounded_String;
            Inst : Instruction;
         begin
            Line := Get_Line(Fin);
            Parse_Line(Line, Inst);
            IV.Append(Inst);
         end;
      end loop;
      Close (Fin);
   end Read_File;
   function Simulate (Instructions : in Vector; Normal : out Boolean) return Integer
   is
      PC : Natural := 0;
      Accumulator : Integer := 0;
      Run : Set;
   begin
      Normal := False;
      loop
         if Run.Contains(PC)
         then
            return Accumulator;
         else
            Run.Insert(PC);
         end if;
   
         case Instructions(PC).Op is
            when Nop =>
               PC := PC + 1;
            when Jmp =>
               PC := PC + Instructions(PC).Value;
            when Acc =>
               Accumulator := Accumulator + Instructions(PC).Value;
               PC := PC + 1;
         end case;
         exit when PC = Integer(Instructions.Length);
      end loop;
      Normal := True;
      return Accumulator;
   end Simulate;
   procedure Run is
      Instructions : Vector;
      Normal_Finish : Boolean;
      Result : Integer;
   begin
      Read_File (Instructions);
      Put_Line("Advent of Code 2020 Day 08 -"); New_Line;
      Result := Simulate(Instructions, Normal_Finish);
      Put_Line("The result for Part 1 is: " & Result'Image);
      Normal_Finish := False;
      for C in Instructions.Iterate loop
         case Instructions(C).Op is
            when Nop =>
               Instructions(C).Op := Jmp;
            when Jmp =>
               Instructions(C).Op := Nop;
            when Acc =>
               null;
         end case;
         if Instructions(C).Op /= Acc
         then
            Result := Simulate(Instructions, Normal_Finish);
            if Normal_Finish
            then
               Put_Line("The result for Part 2 is: " & Result'Image);
               exit;
            end if;
         end if;
         case Instructions(C).Op is
            when Nop =>
               Instructions(C).Op := Jmp;
            when Jmp =>
               Instructions(C).Op := Nop;
            when Acc =>
               null;
         end case;
      end loop;
   end Run;
end AOC2020.Day08;
