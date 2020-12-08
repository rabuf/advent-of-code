with Ada.Containers.Ordered_Maps;  -- 1
with Text_IO; use Text_IO;         -- 2
with Ada.Strings.Fixed;            -- 3
use Ada.Strings.Fixed;
package body AOC2020.Day07 is
   subtype Description_T is String(1..80);
   package Content_Maps is new Ada.Containers.Ordered_Maps
     (Element_Type => Natural,
      Key_Type => Description_T);
   package Bag_Maps is new Ada.Containers.Ordered_Maps
     (Element_Type => Content_Maps.Map,
      Key_Type => Description_T,
      "=" => Content_Maps."=");
   function Make_Description(S : String) return Description_T is
     D : Description_T;
   begin
      D := (others => ' ');
      D(S'First..S'Last) := S;
      return D;
   end Make_Description;

   procedure Parse_Line (Line : String; D : out Description_T; C : out Content_Maps.Map) is
      I, J, K : Natural := 0;
      Bag_Description : Description_T;
   begin
      I := Index (Line, "bags contain", 1);
      D := (others => ' ');
      D(1..I-2) := Line(Line'First..I-2);
      I := I + 13;
      loop
         J := Index (Line, "bag", I);
         if "no other" = Line(I..J-2) then
            C := Content_Maps.Empty_Map;
            exit;
         else
            K := Index (Line, " ", I);
            Bag_Description := (others => ' ');
            Bag_Description(1..(J-2-K)) := Line (K+1..J-2);
            C.Insert(Bag_Description, Integer'Value(Line(I..K-1)));
         end if;
         I := Index (Line, ",", J);
         exit when I = 0;
         I := I + 2;
      end loop;
   end Parse_Line;
   procedure Read_File (Bags : out Bag_Maps.Map) is
      Fin : File_Type;
      Line : String (1..300);
      Length : Natural;
      D : Description_T;
      C : Content_Maps.Map;
   begin
      Open (Fin, In_File, "../input/07.txt");
      while not End_Of_File (Fin) loop
         Get_Line(Fin, Line, Length);
         Parse_Line(Line(1..Length), D, C);
         Bags.Insert (D, Content_Maps.Copy(C));
         C.Clear;
      end loop;
      Close (Fin);
   end Read_File;
   function Recursive_Locate (Bags : in Bag_Maps.Map; Current, Target : Description_T)
                             return Boolean is
   begin
      if Bags(Current).Contains(Target) then return True; end if;
      for B in Bags(Current).Iterate loop
         if Recursive_Locate(Bags, Content_Maps.Key(B), Target)
         then return True;
         end if;
      end loop;
      return False;
   end Recursive_Locate;
   function Number_Of_Bags_That_Hold (Bags : in Bag_Maps.Map; D : Description_T)
                                     return Natural is
      Count : Natural := 0;
   begin
      for B in Bags.Iterate loop
         if Bag_Maps.Key(B) = D then null;
         elsif Recursive_Locate(Bags, Bag_Maps.Key(B), D)
         then Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Number_Of_Bags_That_Hold;
   function Recursive_Count (Bags : in Bag_Maps.Map; Current : Description_T)
                             return Integer is
      Count : Natural := 0;
   begin
      for B in Bags(Current).Iterate loop
         Count := Count + (1 + Recursive_Count(Bags, Content_Maps.Key(B)))
           * Bags(Current)(Content_Maps.Key(B));
      end loop;
      return Count;
   end Recursive_Count;
   procedure Run is
      Bags : Bag_Maps.Map;
      D : Description_T := Make_Description("shiny gold");
   begin
      Read_File(Bags);
      Put_Line("Advent of Code 2020 - Day 07"); New_Line;
      Put_Line("The result for Part 1 is: " & Number_Of_Bags_That_Hold(Bags, D)'Image); 
      Put_Line("The result for Part 2 is: " & Recursive_Count(Bags, D)'Image);
   end Run;
end AOC2020.Day07;
