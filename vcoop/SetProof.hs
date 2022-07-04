module Original where

{-@ LIQUID "--ple" @-}
{-@ LIQUID "--fast" @-}
import Language.Haskell.Liquid.ProofCombinators

proof :: Set -> Set -> Proof

data Set = Empty | Tree Int Set Set

{-@ infix \/ @-}
{-@ measure \/ :: Set -> Set -> Set @-}
{-@ assume \/ :: a : Set -> b : Set -> { v : Set | v = a \/ b } @-}
(\/) :: Set -> Set -> Set
a \/ b = undefined


{-@ infix /\ @-}
{-@ measure /\ :: Set -> Set -> Set @-}
{-@ assume /\ :: a : Set -> b : Set -> { v : Set | v = a /\ b } @-}
(/\) :: Set -> Set -> Set
a /\ b = undefined

{-@ measure emptySet :: Set @-}
{-@ assume emptySet :: {v : Set | v = emptySet} @-}
emptySet :: Set
emptySet = Empty


{-======================================================
    Definitions
=======================================================-}

{-@ predicate IsDisjoint M1 M2 = M1 /\ M2 = emptySet @-}

{-@ disjoint :: s0 : Set -> { s1 : Set | IsDisjoint s0 s1} -> { s0 /\ s1 = emptySet } @-}
disjoint :: Set -> Set -> ()
disjoint _ _ = ()

{-======================================================
    Axioms
=======================================================-}

{-@ assume unionIntersect :: s0 : Set -> s1 : Set -> s2 : Set -> { (s0 \/ s1) /\ s2 = (s0 /\ s2) \/ (s1 /\ s2) } @-}
unionIntersect :: Set -> Set -> Set -> Proof
unionIntersect _ _ _ = ()

{-@ assume intersectSelf :: s0 : Set -> { s0 /\ s0 = s0 } @-}
intersectSelf :: Set -> Proof
intersectSelf _ = ()

{-@ assume intersectComm :: ma : Set -> mb : Set -> {v : () | ma /\ mb = mb /\ ma } @-}
intersectComm :: Set  -> Set -> Proof
intersectComm _ _ = ()

{-@ assume unionEmpty :: ma : Set -> {v : () | ma \/ emptySet = ma } @-}
unionEmpty :: Set -> Proof
unionEmpty _ = ()

{-======================================================
    Proof
=======================================================-}

{-@ proof :: s0 : Set -> { s1 : Set | IsDisjoint s0 s1 } -> { (s0 \/ s1) /\ s0 = s0 } @-}
proof s0 s1 = ???