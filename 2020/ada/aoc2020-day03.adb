with Text_IO; use Text_IO;
package body AOC2020.Day03 is
   type Col_Size is mod 31;
   type Row_Size is mod 323;
   type Unsigned_Integer is mod 2**32;

   type Grid is array (Row_Size, Col_Size) of Boolean;

   Input : Grid;

   function Sled (G : Grid; Down : Row_Size; Over : Col_Size) return Integer is
      Row : Row_Size := 0;
      Col : Col_Size := 0;
      Count : Integer := 0;
      Term : Boolean := False;
   begin
      loop
         -- exit if the next step wraps around
         Term := Row + Down < Row;
         if G (Row, Col)
         then
            Count := Count + 1;
         end if;
         Row := Row + Down;
         Col := Col + Over;
         exit when Term;
      end loop;
      return Count;
   end Sled;

   function Solve_01 return Integer is
   begin
      return Sled (Input, 1, 3);
   end Solve_01;

   function solve_02 return Unsigned_Integer is
   begin
      return Unsigned_Integer(Sled (Input, 1, 1))
        * Unsigned_Integer(Sled (Input, 1, 3))
        * Unsigned_Integer(Sled (Input, 1, 5))
        * Unsigned_Integer(Sled (Input, 1, 7))
        * Unsigned_Integer(Sled (Input, 2, 1));
   end solve_02;

   procedure Parse_Input is
      Line : String (1..32);
      Input_File : file_type;
      Length : Natural;
      Row_Number : Row_Size := 0;
   begin
      Open (Input_File, in_file, "../input/03.txt");
      loop
         exit when end_of_file (Input_File);
         Get_Line (input_file, Line, Length);
         for C in 0..30 loop
            if Line(C+1) = '#' then
               Input (Row_Number, Col_Size(C)) := True;
            else
               Input (Row_Number, Col_Size(C)) := False;
            end if;
         end loop;
         Row_Number := Row_Number + 1;
      end loop;
      Close (Input_File);
   end Parse_Input;
   procedure Run is
   begin
      Parse_Input;
      Put_Line("Advent of Code 2020 - Day 03:"); New_Line;
      Put_Line("The result for part 1 is: " & Integer'Image(Solve_01));
      Put_Line("The result for Part 2 is: " & Unsigned_Integer'Image(Solve_02));
   end Run;
end AOC2020.Day03;
