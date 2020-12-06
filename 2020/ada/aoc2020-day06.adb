with Text_IO; use Text_IO;
package body AOC2020.Day06 is
   procedure Sum_Of_Anyones_Answers is
      F : File_Type;
      Count, T : Natural := 0;
      Answers : array (Character range 'a' .. 'z') of Boolean
        := (others => False);
      C : Character;
   begin
      Open (F, In_File, "../input/06.txt");
      loop
         if End_Of_Line (F) or End_Of_File (F)
         then
            if End_Of_Line (F) then Skip_Line(F); end if;
            if End_Of_Line (F) or End_Of_File(F)
            then
               T := 0;
               for B of Answers loop
                  if B
                  then T := T + 1;
                  end if;
               end loop;
               Count := Count + T;
               Answers := (others => False);
               exit when End_Of_File(F);
            end if;
         end if;
         Get (F, C);
         Answers (C) := True;
      end loop;
      Close (F);
      Put_Line ("The result for Part 1 is: " & Count'Image);
   end Sum_Of_Anyones_Answers;

   procedure Sum_Of_Everyones_Answers is
      F : File_Type;
      Count, T : Natural := 0;
      Answers : array (Character range 'a' .. 'z') of Integer
        := (others => 0);
      Group_Count : Natural := 0;
      C : Character;
   begin
      Open (F, In_File, "../input/06.txt");
      --Open (F, In_File, "test6.txt");
      loop
         if End_Of_Line (F) or End_Of_File (F)
         then
            if End_Of_Line (F) then Skip_Line (F); end if;
            Group_Count := Group_Count + 1;
            if End_Of_Line (F) or End_Of_File (F)
            then
               T := 0;
               for I of Answers loop
                  if I = Group_Count
                  then T := T + 1;
                  end if;
               end loop;
               Count := Count + T;
               Group_Count := 0;
               Answers := (others => 0);
               exit when End_Of_File(F);
            end if;
         end if;
         Get (F, C);
         Answers (C) := Answers (C) + 1;
      end loop;
      Close (F);
      Put_Line("The result for Part 2 is: " & Count'Image);
   end Sum_Of_Everyones_Answers;

   procedure Run is
   begin
      Put_Line("Advent of Code 2020 - Day 06:"); New_Line;
      Sum_Of_Anyones_Answers;
      Sum_Of_Everyones_Answers;
   end Run;
end AOC2020.Day06;
