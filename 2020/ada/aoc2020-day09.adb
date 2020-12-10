with Text_IO; use Text_IO;
with Ada.Text_IO;
with Ada.Containers.Vectors;
package body AOC2020.Day09 is
   package Long_Long_Integer_Vectors is new Ada.Containers.Vectors
     (Element_Type => Long_Long_Integer,
      Index_Type => Natural);
   
   use Long_Long_Integer_Vectors;
   package Long_Long_Integer_Text_IO is new Ada.Text_IO.Integer_IO(Long_Long_Integer);
   use Long_Long_Integer_Text_IO;
   procedure Read_File (Data : out Vector) is
      Fin : File_Type;
      N : Long_Long_Integer;
   begin
      Open (Fin, In_File, "../input/09.txt");
      while not End_Of_File (Fin) loop
         Get(Fin, N);
         Data.Append(N);
      end loop;
      Close (Fin);
   end Read_File;
   function Is_Valid_Code (Data : Vector; I : Natural) return Boolean is
   begin
      for J in I - 26 .. I - 1 loop
         for K in J + 1 .. I - 1 loop
            if J /= K and Data(I) = Data(J) + Data(K)
            then return True;
            end if;
         end loop;
      end loop;
      return False;
   end Is_Valid_Code;
   function Invalid_Code (Data : Vector) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
   begin
      for I in Data.First_Index + 26 .. Data.Last_Index loop
         if not Is_Valid_Code(Data, I)
           then return Data(I);
         end if;
      end loop;
      return Result;
   end Invalid_Code;
   function Find_Block (Data : Vector; Target : Long_Long_Integer) return Long_Long_Integer is
      Min, Max, Sum : Long_Long_Integer := 0;
      I, J : Natural;
   begin
      I := Data.First_Index;
      J := I + 1;
      Sum := Data(I) + Data(J);
      while Sum /= Target and J <= Data.Last_Index loop
         if Target < Sum then
            Sum := Sum - Data(I);
            I := I + 1;
         end if;
         if I = J or Sum < Target then
            J := J + 1;
            Sum := Sum + Data(J);
         end if;
      end loop;
      Min := Data(I);
      Max := Data(I);
      for K in I+1 .. J loop
         Min := Long_Long_Integer'Min(Data (J), Min);
         Max := Long_Long_Integer'Max(Data (J), Max);
      end loop;
      return Min + Max;
   end Find_Block;
   procedure Run is
     Data : Vector;
     Invalid : Long_Long_Integer;
   begin
      Read_File (Data);
      Put_Line ("Advent of Code 2020 Day 09 -"); New_Line;
      Invalid := Invalid_Code(Data);
      Put_Line ("The result for Part 1 is: " & Invalid'Image);
      Put_Line ("The result for Part 2 is: " & Find_Block(Data, Invalid)'Image);
   end Run;
end AOC2020.Day09;
