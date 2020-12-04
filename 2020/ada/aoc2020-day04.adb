with GNAT.Regpat; use GNAT.Regpat;
with Text_IO; use Text_IO;
package body AOC2020.Day04 is
   -- Used as an example of matching regular expressions
   procedure Parse_Line (Line : Unbounded_String; P : out Password) is
      Pattern : constant String := "(\d+)-(\d+) ([a-z]): ([a-z]+)";
      Re : constant Pattern_Matcher := Compile(Pattern);
      Matches : Match_Array (0..4);
      Pass : Unbounded_String;
      P0, P1 : Positive;
      C : Character;
   begin
      Match(Re, To_String(Line), Matches);
      P0 := Integer'Value(Slice(Line, Matches(1).First, Matches(1).Last));
      P1 := Integer'Value(Slice(Line, Matches(2).First, Matches(2).Last));
      C := Element(Line, Matches(3).First);
      Pass := To_Unbounded_String(Slice(Line, Matches(4).First, Matches(4).Last));
      P := (Min_Or_Pos => P0,
            Max_Or_Pos => P1,
            C => C,
            P => Pass);
   end Parse_Line;
begin
   null;
end AOC2020.Day04;
