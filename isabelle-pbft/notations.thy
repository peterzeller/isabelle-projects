theory notations
  imports Main
begin

section \<open>Notations\<close>

text \<open>This section introduces some notation for the rest of the paper.\<close>

abbreviation equals_the (infix "\<triangleq>" 55) where
\<open>x \<triangleq> y \<equiv> x = Some y\<close>

end