import qualified Data.Tree as Tree

farey n = Tree.flatten $ Tree.unfoldTree fareyDeeper (0, 1, 1, 1, n)
fareyDeeper (p1,q1,p3,q3,n) = ((p2,q2), nextTree)
    where p2 = p1 + p3
          q2 = q1 + q3
          nextTree = (if q1+q2>n then [] else (p1,q1,p2,q2,n):[]) ++
                     (if q2+q3>n then [] else (p2,q2,p3,q3,n):[])