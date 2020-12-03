with Ada.Containers.Vectors;
with Text_Io; use Text_Io;
with Ada.Text_Io.Unbounded_Io; use Ada.Text_Io.Unbounded_Io;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body AOC2020.Day02 is

   type Password is record
      Min_Or_Pos : Positive;
      Max_Or_Pos : Positive;
      C : Character;
      P : Unbounded_String;
   end record;

   package Password_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Password);
   use Password_Vectors;

   Input : Vector;

   function Old_Valid(P : Password) return Boolean is
      Count : Natural := 0;
   begin
      for I in 1..Length(P.P) loop
         if P.C = Element (P.P, I)
         then
            Count := Count + 1;
         end if;
      end loop;
      return (P.Min_Or_Pos <= Count) and (Count <= P.Max_Or_Pos);
   end Old_Valid;

   function Solve_01 return Integer is
      Result : Integer := 0;
   begin
      for I in Input.Iterate loop
         if Old_Valid(Input(I))
         then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Solve_01;

   function New_Valid(P : Password) return Boolean is
   begin
      return (Element (P.P, P.Min_Or_Pos) = P.C xor Element (P.P, P.Max_Or_Pos) = P.C);
   end New_Valid;
   function Solve_02 return Integer is
      Result : Integer := 0;
   begin
      for I in Input.Iterate loop
         if New_Valid(Input(I))
         then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Solve_02;

   procedure Parse_Line (Line : Unbounded_String; P : out Password) is
      Pass : Unbounded_String;
      I,J : Positive;
      P0, P1 : Positive;
      C : Character;
   begin
      I := Index(Line, "-");
      J := Index(Line, " ");
      P0 := Integer'Value(Slice(Line, 1, I-1));
      P1 := Integer'Value(Slice(Line, I+1, J-1));
      C := Element(Line, J+1);
      Pass := To_Unbounded_String(Slice(Line, J+4, Length(Line)));
      P := (Min_Or_Pos => P0,
            Max_Or_Pos => P1,
            C => C,
            P => Pass);
   end Parse_Line;

begin
   declare
      Input_File : file_type;
      Line : Unbounded_String;
      P : Password;

   begin
      Open (Input_File, in_file, "../input/02.txt");
      loop
         exit when end_of_file (Input_File);
         Get_Line (input_file, Line);
         Parse_Line(Line, P);
         Input.Append (P);
      end loop;
   end;
   Put_Line("Advent of Code 2020 - Day 02:"); New_Line;
   Put_Line("The result for part 1 is: " & Integer'Image(Solve_01));
   Put_Line("The result for Part 2 is: " & Integer'Image(Solve_02));
end AOC2020.Day02;
