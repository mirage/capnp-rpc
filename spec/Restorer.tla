 ------------------------------ MODULE Restorer ------------------------------

EXTENDS Integers, TLC, Sequences, TLAPS, FiniteSets

(*
The resolve function is used to get a live capability from a sturdy ID.
If the cap is already loaded in memory, it returns that.
Otherwise, if it is currently being loaded, it waits for that to finish and returns it.
Otherwise, if it is not loaded or being loaded, it loads it and returns the new cap.

In any case, the caller gets a ref on the new capability and is responsible for calling
dec_ref on it later. When the ref-count reaches zero, the cap must be removed from memory.

We must ensure that we never have two live caps for the same sturdy ID at once,
and that we never free a cap while a user has a reference to it.

The tricky case is:
- Two callers ask for the same cap together.
- We return it to one caller with rc=1 first.
- This caller immediately frees it.
- We try to return it to the other caller, but can't because rc=0.

To simplify the model, we only consider a single sturdy ID.
This is reasonable, as calls for objects with different IDs don't interact with each other.
*)

-------------------------------------------------------------------------------

(* The maximum number of simultaneous callers to consider.
   For model checking this is set to a small number,
   but the proofs only assume that it is finite. *) 
CONSTANT nProcs
ASSUME NumProcsNat == nProcs \in Nat

(* The possible states of the in-memory cache:
   - it contains a loaded capability, represented by its ref-count,
   - it contains a promise for a capability that is currently being loaded,
   - it (briefly) contains the error from a failed load,
   - it is unused (neither loaded nor being loaded) *)
resolved == Nat \ {0}
CONSTANT promise
CONSTANT failed
CONSTANT unused

ASSUME distinct ==
  /\ unused \notin resolved
  /\ promise \notin {unused} \cup resolved
  /\ failed \notin {unused, promise} \cup resolved

(* --algorithm Restorer
     \* rc represents the state of the in-memory cache.
     variables rc = unused;
     fair process Proc \in 1..nProcs
       \* A process at "start" is not using the capability.
       begin start:-
       while (TRUE) do
         \* The process has decided it wants to use the cap and called into the loader.
         if rc \in resolved then
           \* In this case, we MUST inc_ref immediately.
           rc := rc + 1;
         else
           if rc = unused then
             \* Not loaded or being loaded. Begin a new load operation.
             \* Put a promise of the result in the cache.
             rc := promise;
             \* Loading happens asynchronously...
             loaded:
             assert rc = promise;
             either
               rc := 1;
             or
               rc := failed;
               load_failed:
               \* Give other processes a chance to see that the load failed
               \* before removing it from the cache.
               \* (this wait is just an artifact of the modelling)
               await ~\E p \in ProcSet : pc[p] \in {"wait2"};
               assert rc = failed;
               rc := unused;
               goto start;
             end either;
             \* We get woken up at some point after it resolves, taking the initial ref-count.
             wait1:
             assert rc \in resolved;
             \* We need to handle all the ref-counting before giving the cap to our user,
             \* since once the user has it they can free it.
             \* We call Lwt.pause to ensure anyone waiting has a chance to inc-ref.
             \* When this returns, all ref-counts will be correct:
             \* - Everyone who was waiting has incremented the ref-count.
             \* - Anyone who tried to get a cap during this period found the resolved cap
             \*   and inc-ref'd that.
             \* Noone has been able to drop the ref-count to zero because we still own the
             \* initial ref-count of 1 that the object had when it was created.
             pause:
             \* Binds run immediately, and noone can enter wait2 now (see NoEnterWait), so
             \* this simulates the Lwt.pause: everyone who was waiting for the promise to
             \* resolve has now inc-ref'd it.
             await ~\E p \in ProcSet : pc[p] \in {"wait2"};
           else
             \* A load is already in progress (or a load recently failed).
             \* We'll get woken up at some point after it finishes and add a ref.
             \* We know that the loading thread hasn't reached "pause" yet,
             \* because if it had then rc wouldn't be a promise now.
             wait2:
             \* We got worken up because rc is no longer a promise.
             await rc # promise;
             if rc \notin resolved then
               assert rc = failed;
               goto start;
             else
               rc := rc + 1;
             end if;
             \* We can safely return the reference to the user now.
             \* Although there may be other processes who want to call inc_ref too,
             \* we know our caller can't drop the last reference to it -
             \* they can only drop the reference we just added above.
           end if;
         end if;
         \* At this point, the cap is resolved and we return it to the user code.
         \* Eventually, it may decide to free it...
         release:-
         \* The user has called dec-ref.
         if rc = 1 then
           rc := unused;    \* Remove from cache when ref-count drops to zero.
         else
           rc := rc - 1;
         end if;
       end while;
     end process;
   end algorithm;
*) 

\* BEGIN TRANSLATION
VARIABLES rc, pc

vars == << rc, pc >>

ProcSet == (1..nProcs)

Init == (* Global variables *)
        /\ rc = unused
        /\ pc = [self \in ProcSet |-> "start"]

start(self) == /\ pc[self] = "start"
               /\ IF rc \in resolved
                     THEN /\ rc' = rc + 1
                          /\ pc' = [pc EXCEPT ![self] = "release"]
                     ELSE /\ IF rc = unused
                                THEN /\ rc' = promise
                                     /\ pc' = [pc EXCEPT ![self] = "loaded"]
                                ELSE /\ pc' = [pc EXCEPT ![self] = "wait2"]
                                     /\ rc' = rc

release(self) == /\ pc[self] = "release"
                 /\ IF rc = 1
                       THEN /\ rc' = unused
                       ELSE /\ rc' = rc - 1
                 /\ pc' = [pc EXCEPT ![self] = "start"]

loaded(self) == /\ pc[self] = "loaded"
                /\ Assert(rc = promise, 
                          "Failure of assertion at line 68, column 14.")
                /\ \/ /\ rc' = 1
                      /\ pc' = [pc EXCEPT ![self] = "wait1"]
                   \/ /\ rc' = failed
                      /\ pc' = [pc EXCEPT ![self] = "load_failed"]

load_failed(self) == /\ pc[self] = "load_failed"
                     /\ ~\E p \in ProcSet : pc[p] \in {"wait2"}
                     /\ Assert(rc = failed, 
                               "Failure of assertion at line 77, column 16.")
                     /\ rc' = unused
                     /\ pc' = [pc EXCEPT ![self] = "start"]

wait1(self) == /\ pc[self] = "wait1"
               /\ Assert(rc \in resolved, 
                         "Failure of assertion at line 83, column 14.")
               /\ pc' = [pc EXCEPT ![self] = "pause"]
               /\ rc' = rc

pause(self) == /\ pc[self] = "pause"
               /\ ~\E p \in ProcSet : pc[p] \in {"wait2"}
               /\ pc' = [pc EXCEPT ![self] = "release"]
               /\ rc' = rc

wait2(self) == /\ pc[self] = "wait2"
               /\ rc # promise
               /\ IF rc \notin resolved
                     THEN /\ Assert(rc = failed, 
                                    "Failure of assertion at line 107, column 16.")
                          /\ pc' = [pc EXCEPT ![self] = "start"]
                          /\ rc' = rc
                     ELSE /\ rc' = rc + 1
                          /\ pc' = [pc EXCEPT ![self] = "release"]

Proc(self) == start(self) \/ release(self) \/ loaded(self)
                 \/ load_failed(self) \/ wait1(self) \/ pause(self)
                 \/ wait2(self)

Next == (\E self \in 1..nProcs: Proc(self))
           \/ (* Disjunct to prevent deadlock on termination *)
              ((\A self \in ProcSet: pc[self] = "Done") /\ UNCHANGED vars)

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in 1..nProcs : WF_vars((pc[self] \notin {"start", "release"}) /\ Proc(self))

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

-------------------------------------------------------------------------------

\* Some obvious (and hopefully correct) facts about sets.

THEOREM FiniteSubset ==
  \A S : \A T \in SUBSET S : IsFiniteSet(S) => IsFiniteSet(T)

THEOREM CardPlus1 ==
  \A S : \A x \in S : IsFiniteSet(S) => Cardinality(S) = Cardinality(S \ {x}) + 1
  
THEOREM CardEmpty ==
  \A S : IsFiniteSet(S) => (Cardinality(S) = 0 <=> S = {})

THEOREM CardFinite ==
  \A S : \A T \in SUBSET S : IsFiniteSet(S) => Cardinality(T) \in Nat

THEOREM IntRangeFinite ==
  \A x, y \in Int : IsFiniteSet(x..y)
  
-----------------------------------------------------------------------------

\* Invariants to check. 

(* The number of processes that think they own a ref-count.
   A "wait1" process owns the initial ref-count. A "wait2" one hasn't yet taken a ref. *)
Users ==
  Cardinality( { p \in ProcSet : pc[p] \in {"wait1", "pause", "release"} } )

\* The actual ref-count on the object.
Count ==
  IF rc \in resolved THEN rc
  ELSE 0 

(* An inductive invariant for the system that checks Count = Users.
   Note: if you want to check IInv /\ Next => IInv', set unused, promise and failed
   to e.g. -10/-20/-30 in the model. *)
IInv ==
\* Types
  /\ rc \in {unused, promise, failed} \cup resolved
  /\ pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release", "load_failed"} ]
\* Ref-counts are correct
  /\ Count = Users
\* If rc is a promise, someone is working to resolve it
  /\ rc = promise => \E p \in ProcSet : pc[p] \in {"loaded", "wait1", "pause"}
\* If loading failed, someone is working to remove it from the cache
  /\ rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"}
\* We never have two processes both loading at the same time
  /\ \A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q
\* If we are loading, rc is a promise
  /\ (\E p \in ProcSet : pc[p] \in {"loaded"}) => rc = promise
\* If someone is waiting for it to load, someone is loading it
  /\ (\E p \in ProcSet : pc[p] \in {"wait2"}) => \E q \in ProcSet : pc[q] \in {"loaded", "wait1", "pause", "load_failed"}

\* A process will not enter the wait2 state while another process is in the pause state.
NoEnterWait ==
  \A p \in ProcSet : pc[p] # "wait2" /\ (\E q \in ProcSet : pc[q] = "pause") => pc'[p] # "wait2" 


-----------------------------------------------------------------------------

\* Proofs.

LEMMA ProcSetFinite == IsFiniteSet(ProcSet)
  BY NumProcsNat, IntRangeFinite DEF ProcSet  

LEMMA IncUsers ==
   ASSUME NEW self \in ProcSet, 
          /\ IInv
          /\ pc' = [pc EXCEPT ![self] = pc'[self]]
          /\ \/ (rc \in resolved /\ rc' = rc + 1)
             \/ (rc \in {unused, promise, failed} /\ rc' = 1)
          /\ pc[self] \notin {"wait1", "pause", "release"}
          /\ pc'[self] \in {"wait1", "pause", "release"}
  PROVE
    Users' = Count'
<1>1. Count = Users
      BY DEF IInv
<1>2. Users' = Users + 1
      <2> self \in { p \in ProcSet : pc'[p] \in {"wait1", "pause", "release"} }
          OBVIOUS 
      <2> Cardinality( { p \in ProcSet : pc'[p] \in {"wait1", "pause", "release"} } ) =
          Cardinality( { p \in ProcSet \ {self} : pc'[p] \in {"wait1", "pause", "release"} } ) + 1
          BY FiniteSubset, ProcSetFinite, CardPlus1
      <2> \A p \in ProcSet \ {self} : pc[p] = pc'[p]
          BY DEF IInv
      <2> { p \in ProcSet \ {self} : pc'[p] \in {"wait1", "pause", "release"} } =
          { p \in ProcSet \ {self} : pc[p] \in {"wait1", "pause", "release"} }
          OBVIOUS
      <2> QED
          BY DEF Users
<1>3. Count' = Count + 1
      <2> CASE rc \in resolved /\ rc' = rc + 1
          BY DEF Count, resolved
      <2> CASE rc \in {unused, promise, failed} /\ rc' = 1
          <3> Count = 0
              BY distinct DEF Count
          <3> Count' = 1
              BY DEF Count, resolved
          <3> QED
              OBVIOUS
      <2> QED
          OBVIOUS
<1> QED
    BY <1>1, <1>2, <1>3
    
LEMMA DecUsers ==
   ASSUME NEW self \in ProcSet, 
          /\ IInv
          /\ pc' = [pc EXCEPT ![self] = pc'[self]]
          /\ rc \in resolved
          /\ \/ rc' = rc - 1
             \/ (rc = 1 /\ rc' \in {unused, promise})
          /\ pc[self] \in {"wait1", "pause", "release"}
          /\ pc'[self] \notin {"wait1", "pause", "release"}
  PROVE
    Users' = Count'
<1>1. Count = Users
      BY DEF IInv
<1>2. Users' = Users - 1
      <2> self \in { p \in ProcSet : pc[p] \in {"wait1", "pause", "release"} }
          OBVIOUS 
      <2> Cardinality( { p \in ProcSet : pc[p] \in {"wait1", "pause", "release"} } ) =
          Cardinality( { p \in ProcSet \ {self} : pc[p] \in {"wait1", "pause", "release"} } ) + 1
          BY FiniteSubset, ProcSetFinite, CardPlus1
      <2> \A p \in ProcSet \ {self} : pc[p] = pc'[p]
          BY DEF IInv
      <2> { p \in ProcSet \ {self} : pc'[p] \in {"wait1", "pause", "release"} } =
          { p \in ProcSet \ {self} : pc[p] \in {"wait1", "pause", "release"} }
          OBVIOUS
      <2> Users = Users' + 1
          BY DEF Users
      <2> { p \in ProcSet \ {self} : pc[p] \in {"wait1", "pause", "release"} } \subseteq ProcSet
          OBVIOUS
      <2> Users' \in Nat
          BY ProcSetFinite, CardFinite DEF Users
      <2> QED
          OBVIOUS
<1>3. Count' = Count - 1
      <2> CASE rc' = rc - 1
          BY DEF Count, resolved
      <2> CASE rc = 1 /\ rc' \in {unused, promise}
          <3> Count = 1
              BY distinct DEF Count
          <3> Count' = 0
              BY distinct DEF Count
          <3> QED
              OBVIOUS
      <2> QED
          OBVIOUS
<1> QED
    BY <1>1, <1>2, <1>3
  
LEMMA SameUsers ==
   ASSUME NEW self \in ProcSet,
          /\ IInv
          /\ pc' = [pc EXCEPT ![self] = pc'[self]]
          /\ \/ rc' = rc
             \/ (rc \in {unused, promise, failed} /\ rc' \in {unused, promise, failed}) 
          /\ (pc[self] \in {"wait1", "pause", "release"}) = (pc'[self] \in {"wait1", "pause", "release"})
  PROVE
    Users' = Count'
<1>1. Users' = Users
      <2> ASSUME NEW p \in ProcSet
                 PROVE (pc[p] \in {"wait1", "pause", "release"}) = (pc'[p] \in {"wait1", "pause", "release"})
          <3> CASE p = self
              OBVIOUS
          <3> CASE p # self
              BY pc[p] = pc'[p] DEF IInv
          <3> QED
              OBVIOUS
      <2> { p \in ProcSet : pc[p] \in {"wait1", "pause", "release"} } =
          { p \in ProcSet : pc'[p] \in {"wait1", "pause", "release"} }
          OBVIOUS
      <2> QED
          BY DEF Users  
<1>2. Count' = Count
      BY distinct DEF Count, resolved
<1> QED
    BY <1>1, <1>2, Count = Users DEF IInv 
      
LEMMA LoneLoader ==
   ASSUME NEW self \in ProcSet, 
   /\ IInv
   /\ pc' = [pc EXCEPT ![self] = pc'[self]]
   /\ \/ pc'[self] \notin {"loaded", "load_failed"}
      \/ ~\E p \in ProcSet : pc[p] \in {"loaded", "load_failed"}
   PROVE (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
<1>1. CASE pc'[self] # "loaded"
      BY DEF IInv
<1>2. CASE ~\E p \in ProcSet : pc[p] \in {"loaded", "load_failed"}
      <2> SUFFICES ASSUME NEW p \in ProcSet,
                          NEW q \in ProcSet,
                          pc'[p] \in {"loaded", "load_failed"} /\ pc'[q] \in {"loaded", "load_failed"},
                          p # q
                   PROVE FALSE
          OBVIOUS
      <2> pc[p] # "loaded" /\ pc[q] # "loaded"
          BY <1>2
      <2> QED
          BY DEF IInv
<1> QED
    BY <1>1, <1>2

LEMMA HasUsers ==
  ASSUME NEW p \in ProcSet, pc[p] \in {"wait1", "pause", "release"},
         IInv
  PROVE  rc \in resolved
<1> Users > 0
    <2> SUFFICES ASSUME Users = 0
                 PROVE  FALSE
        BY ProcSetFinite, CardFinite DEF Users
    <2> Cardinality( {q \in ProcSet : pc[q] \in {"wait1", "pause", "release"} } ) = 0
        BY DEF Users
    <2> {q \in ProcSet : pc[q] \in {"wait1", "pause", "release"} } = {}
        BY ProcSetFinite, FiniteSubset, CardEmpty
    <2> QED
        OBVIOUS
<1> Count > 0
    BY Users = Count DEF IInv
<1> QED
    BY DEF Count

LEMMA FailSame ==
  ASSUME NEW self \in ProcSet, 
         IInv,
         pc' = [pc EXCEPT ![self] = pc'[self]],
         pc'[self] # "load_failed",
         pc[self] # "load_failed",
         (rc' = failed) = (rc = failed)
  PROVE  (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
  BY DEF IInv

  
THEOREM do_start ==
  ASSUME NEW self \in ProcSet, IInv /\ start(self)
  PROVE  IInv'
  <1> /\ pc[self] = "start"
      /\ IF rc \in resolved
                     THEN /\ rc' = rc + 1
                          /\ pc' = [pc EXCEPT ![self] = "release"]
                     ELSE /\ IF rc = unused
                                THEN /\ rc' = promise
                                     /\ pc' = [pc EXCEPT ![self] = "loaded"]
                                ELSE /\ pc' = [pc EXCEPT ![self] = "wait2"]
                                     /\ rc' = rc
         BY DEF start
  <1>   USE DEF IInv
  <1>1. (rc \in {unused, promise, failed} \cup resolved)'
        BY DEF resolved
  <1>2. (pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release", "load_failed"} ])'
        OBVIOUS
  <1>3. (Count = Users)'
        <2> USE distinct DEF resolved
        <2>1. CASE rc \in resolved BY <2>1, IncUsers
        <2>2. CASE rc \in {unused, promise, failed}     BY <2>2, SameUsers
        <2> QED
            BY <2>1, <2>2
  <1>4. (rc = promise => \E p \in ProcSet : pc[p] \in {"loaded", "wait1", "pause"})'
        <2>1. CASE rc = unused
              <3> pc'[self] = "loaded"
                  BY <2>1, distinct DEF IInv
              <3> QED
                  OBVIOUS
        <2>2. CASE rc = promise
              BY <2>2 DEF IInv
        <2> QED
            BY <2>1, <2>2, distinct DEF resolved
  <1>5. (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
        <2> ASSUME pc'[self] \in {"loaded", "load_failed"}, NEW p \in ProcSet, pc[p] \in {"loaded", "load_failed"}
            PROVE  FALSE
            <3> rc = unused 
                BY distinct
            <3> rc \in {promise, failed}
                BY DEF IInv
            <3> QED
                BY distinct
        <2> QED
            BY LoneLoader
  <1>6. ASSUME NEW p \in ProcSet, pc'[p] \in {"loaded"}
        PROVE  rc' = promise
        <2> CASE pc'[self] = "loaded"
            OBVIOUS
        <2> CASE pc'[self] # "loaded"
            <3> pc[p] \in {"loaded"}
                BY <1>6
            <3> rc = promise
                BY DEF IInv
            <3> QED
                BY distinct
        <2> QED 
            OBVIOUS
  <1>7. ASSUME NEW p \in ProcSet, pc'[p] \in {"wait2"}
        PROVE  \E q \in ProcSet : pc'[q] \in {"loaded", "wait1", "pause", "load_failed"}
        <2>1. CASE p = self
              <3> rc \in {promise, failed}
                  BY <1>7, <2>1
              <3> QED
                  BY DEF IInv
        <2>2. CASE p # self
              BY <1>7, <2>2 DEF IInv
        <2> QED
            BY <2>1, <2>2
  <1>x. (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
        BY distinct, FailSame DEF resolved
  <1>8. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>7, <1>x DEF IInv
         
THEOREM do_release ==
  ASSUME NEW self \in ProcSet, IInv /\ release(self)
  PROVE  IInv'
  <1> /\ pc[self] = "release"
      /\ IF rc = 1
            THEN /\ rc' = unused
            ELSE /\ rc' = rc - 1
      /\ pc' = [pc EXCEPT ![self] = "start"]
      BY DEF release
  <1> rc \in resolved
      BY HasUsers
  <1>1. (rc \in {unused} \cup resolved)'
      BY DEF resolved
  <1>2. (pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release", "load_failed"} ])'
      BY DEF IInv
  <1>3. (Count = Users)'
      BY DecUsers DEF resolved, IInv
  <1>4. (rc = promise => \E p \in ProcSet : pc[p] \in {"loaded", "wait1", "pause"})'
      BY distinct, rc' # promise DEF resolved
  <1>5. (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
      BY LoneLoader DEF IInv
  <1>6. ASSUME NEW p \in ProcSet, pc'[p] \in {"loaded"}
        PROVE  FALSE
        <2> pc[p] = "loaded"
            BY <1>6 DEF IInv
        <2> rc = promise
            BY DEF IInv
        <2> QED 
            BY rc \in resolved, distinct
  <1>7. ASSUME NEW p \in ProcSet, pc'[p] \in {"wait2"}
        PROVE  \E q \in ProcSet : pc'[q] \in {"loaded", "wait1", "pause", "load_failed"}
        <2> pc[p] = "wait2"
            BY <1>7 DEF IInv
        <2> \E q \in ProcSet \ {self} : pc'[q] \in {"loaded", "wait1", "pause", "load_failed"}
            BY DEF IInv
        <2> QED
            OBVIOUS
  <1>x. (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
         BY <1>1, FailSame, distinct DEF resolved, IInv
  <1>8. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>7, <1>x DEF IInv

THEOREM do_loaded ==
  ASSUME NEW self \in ProcSet, IInv /\ loaded(self)
  PROVE  IInv'
  <1> /\ pc[self] = "loaded"
      /\ \/ /\ rc' = 1
            /\ pc' = [pc EXCEPT ![self] = "wait1"]
         \/ /\ rc' = failed
            /\ pc' = [pc EXCEPT ![self] = "load_failed"]
      BY DEF loaded
  <1> rc = promise
      BY DEF IInv
  <1>1. (rc \in {unused, promise, failed} \cup resolved)'
      BY DEF resolved
  <1>2. (pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release", "load_failed"} ])'
      BY DEF IInv
  <1>3. (Count = Users)'
      <2> CASE pc'[self] = "wait1"
          BY IncUsers DEF resolved, IInv
      <2> CASE pc'[self] = "load_failed"
          BY SameUsers DEF resolved, IInv
      <2> QED
          OBVIOUS
  <1>4. rc' # promise
      BY distinct DEF resolved
  <1>5. (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
      BY LoneLoader DEF IInv
  <1>6. ASSUME NEW p \in ProcSet, pc'[p] \in {"loaded"}
               PROVE FALSE
        <2> p # self
            BY <1>6 DEF IInv
        <2> pc[p] = "loaded"
            BY <1>6 DEF IInv
        <2> pc[self] = "loaded" /\ self # p
            OBVIOUS
        <2> QED
            BY DEF IInv
  <1>7. ASSUME NEW p \in ProcSet, pc'[p] \in {"wait2"}
        PROVE  \E q \in ProcSet : pc'[q] \in {"loaded", "wait1", "pause", "load_failed"}
        <2> CASE pc'[self] = "wait1" BY DEF IInv
        <2> CASE pc'[self] # "wait1" BY DEF IInv
        <2> QED OBVIOUS
  <1>x. (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
      <2> CASE pc'[self] = "wait1"
          BY FailSame, distinct DEF resolved
      <2> CASE pc'[self] = "load_failed"
          OBVIOUS
      <2> QED
          OBVIOUS
  <1>8. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>7, <1>x DEF IInv

THEOREM do_wait1 ==
  ASSUME NEW self \in ProcSet, IInv /\ wait1(self)
  PROVE  IInv'
  <1> /\ pc[self] = "wait1"
      /\ pc' = [pc EXCEPT ![self] = "pause"]
      /\ rc' = rc
      BY DEF wait1
  <1> pc'[self] = "pause"
      BY DEF IInv
  <1>1. (rc \in {unused, promise, failed} \cup resolved)'
      BY DEF IInv
  <1>2. (pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release", "load_failed"} ])'
      BY DEF IInv
  <1>3. (Count = Users)'
      BY SameUsers
  <1>4. rc # promise
      BY HasUsers, distinct
  <1>5. (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
      BY LoneLoader
  <1>6. ASSUME NEW p \in ProcSet, pc'[p] \in {"loaded"}
        PROVE  FALSE
        <2> pc[p] = "loaded"
            BY <1>6 DEF IInv
        <2> rc = promise
            BY DEF IInv
        <2> QED
            BY <1>4
  <1>7. ASSUME NEW p \in ProcSet, pc'[p] \in {"wait2"}
        PROVE  \E q \in ProcSet : pc'[q] \in {"loaded", "wait1", "pause", "load_failed"}
        BY pc'[self] = "pause"
  <1>x. (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
        BY FailSame
  <1>8. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>7, <1>x DEF IInv
  
THEOREM do_pause ==
  ASSUME NEW self \in ProcSet, IInv /\ pause(self)
  PROVE  IInv'
  <1> /\ pc[self] = "pause"
      /\ ~\E p \in ProcSet : pc[p] \in {"wait2"}
      /\ pc' = [pc EXCEPT ![self] = "release"]
      /\ rc' = rc
      BY DEF pause
  <1> pc'[self] = "release"
      BY DEF IInv
  <1>1. (rc \in {unused, promise, failed} \cup resolved)'
      BY DEF IInv
  <1>2. (pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release", "load_failed"} ])'
      BY DEF IInv
  <1>3. (Count = Users)'
      BY SameUsers
  <1>4. rc \in resolved
      BY HasUsers
  <1>5. (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
      BY LoneLoader
  <1>6. ASSUME NEW p \in ProcSet, pc'[p] \in {"loaded"}
        PROVE  FALSE
        <2> p # self
            BY <1>6
        <2> pc[p] = "loaded"
            BY <1>6 DEF IInv
        <2> rc = promise
            BY DEF IInv
        <2> QED
            BY <1>4, distinct
  <1>7. ASSUME NEW p \in ProcSet, pc'[p] \in {"wait2"}
        PROVE  \E q \in ProcSet : pc'[q] \in {"loaded", "wait1", "pause", "load_failed"}
        <2> pc[p] = "wait2"
            BY <1>7 DEF IInv
        <2> QED
            OBVIOUS
  <1>x. (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
        BY FailSame
  <1>8. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>7, <1>x, distinct DEF IInv

THEOREM do_wait2 ==
  ASSUME NEW self \in ProcSet, IInv /\ wait2(self)
  PROVE  IInv'
  <1> /\ pc[self] = "wait2"
      /\ rc # promise
      /\ IF rc \notin resolved
                     THEN /\ pc' = [pc EXCEPT ![self] = "start"]
                          /\ rc' = rc
                     ELSE /\ rc' = rc + 1
                          /\ pc' = [pc EXCEPT ![self] = "release"]
      BY DEF wait2
  <1> pc'[self] \in {"release", "start"}
      BY DEF IInv
  <1> rc \in resolved \cup {failed}
      BY HasUsers DEF IInv
  <1>1. (rc \in {unused, promise, failed} \cup resolved)'
      BY DEF resolved
  <1>2. (pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release", "load_failed"} ])'
      BY DEF IInv
  <1>3. (Count = Users)'
      <2> CASE pc'[self] = "start" BY SameUsers
      <2> CASE pc'[self] = "release" BY IncUsers
      <2> QED OBVIOUS
  <1>4. rc # promise
        BY distinct
  <1>5. (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
        BY LoneLoader
  <1>6. ASSUME NEW p \in ProcSet, pc'[p] \in {"loaded"}
        PROVE  FALSE
        <2> pc[p] = "loaded"
            BY <1>6 DEF IInv
        <2> QED
            BY rc = promise DEF IInv
  <1>7. ASSUME NEW p \in ProcSet, pc'[p] \in {"wait2"}
        PROVE  \E q \in ProcSet : pc'[q] \in {"loaded", "wait1", "pause", "load_failed"}
          <2> pc[p] = "wait2"
              BY <1>7 DEF IInv
          <2> \E q \in ProcSet \ {self} : pc[q] \in {"loaded", "wait1", "pause", "load_failed"}
              BY DEF IInv
          <2> QED
              BY DEF IInv
  <1>x. (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
        <2> CASE rc \in resolved BY distinct, FailSame DEF resolved
        <2> CASE rc = failed BY FailSame
        <2> QED OBVIOUS
  <1>8. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>7, <1>x DEF IInv

THEOREM do_load_failed ==
  ASSUME NEW self \in ProcSet, IInv /\ load_failed(self)
  PROVE  IInv'
  <1> /\ pc[self] = "load_failed"
      /\ ~\E p \in ProcSet : pc[p] \in {"wait2"}
      /\ rc' = unused
      /\ pc' = [pc EXCEPT ![self] = "start"]
      BY DEF load_failed
  <1>1. rc = failed BY DEF IInv
  <1>2. pc'[self] = "start" BY DEF IInv
  <1>3. (Count = Users)'
        <2> (pc[self]  \in {"wait1", "pause", "release"}) =
            (pc'[self] \in {"wait1", "pause", "release"})
            BY <1>2
        <2> QED BY <1>1, <1>2, SameUsers
  <1>4. rc # promise BY <1>1, distinct
  <1>5. (rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"})'
        <2> rc' # failed
            BY distinct
        <2> ASSUME NEW p \in ProcSet, pc'[p] \in {"load_failed"}
                   PROVE FALSE
                <3> p # self BY <1>2
                <3> pc[p] = "load_failed" BY DEF IInv
                <3> QED BY DEF IInv
        <2> QED
            OBVIOUS
  <1>6. (\A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q)'
        BY <1>2, LoneLoader
  <1>7. ASSUME NEW p \in ProcSet, pc'[p] \in {"loaded"}
        PROVE  FALSE
        <2> pc[p] = "loaded" BY <1>2, <1>7 DEF IInv
        <2> pc[self] \notin {"loaded", "load_failed"} BY DEF IInv
        <2> QED OBVIOUS
  <1>8. ASSUME NEW p \in ProcSet, pc'[p] \in {"wait2"}
        PROVE  FALSE
        BY <1>2, <1>8, pc[p] = "wait2" DEF IInv
  <1>9. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>7, <1>8, distinct DEF IInv

THEOREM NextOK ==
        ASSUME IInv /\ [Next]_vars
        PROVE  IInv'
<1>0. CASE UNCHANGED vars 
      BY <1>0 DEF IInv, Count, Users, vars
<1>   SUFFICES ASSUME NEW self \in ProcSet, Proc(self)
               PROVE  IInv'
      BY <1>0 DEF Next, ProcSet, IInv             
<1>1. CASE start(self)    BY <1>1, do_start
<1>2. CASE release(self)  BY <1>2, do_release
<1>3. CASE loaded(self)   BY <1>3, do_loaded
<1>4. CASE wait1(self)    BY <1>4, do_wait1
<1>5. CASE pause(self)    BY <1>5, do_pause
<1>6. CASE wait2(self)    BY <1>6, do_wait2
<1>x. CASE load_failed(self) BY <1>x, do_load_failed
<1>7. QED
    BY <1>1, <1>2, <1>3, <1>4, <1>5, <1>6, <1>x DEF Proc

THEOREM InvOK == Spec => []IInv
<1>1. Init => IInv
  <2> SUFFICES ASSUME Init
               PROVE  IInv
    OBVIOUS
  <2>1. rc \in {unused, promise} \cup resolved
    BY DEF Init
  <2>2. pc \in [ ProcSet -> {"start", "wait1", "wait2", "pause", "loaded", "release"} ]
    BY DEF Init
  <2>3. Count = Users
    <3> {p \in ProcSet : pc[p] \in {"wait1", "pause", "release"}} = {}
        BY DEF Init
    <3> Users = 0
        BY CardEmpty, ProcSetFinite, FiniteSubset DEF Users
    <3> Count = 0
        BY distinct DEF Init, Count
    <3> QED
        OBVIOUS
  <2>4. rc = promise => \E p \in ProcSet : pc[p] \in {"loaded", "wait1", "pause"}
    BY distinct DEF Init
  <2>5. \A p, q \in ProcSet : pc[p] \in {"loaded", "load_failed"} /\ pc[q] \in {"loaded", "load_failed"} => p = q
    BY DEF Init
  <2>6. (\E p \in ProcSet : pc[p] \in {"loaded"}) => rc = promise
    BY DEF Init
  <2>7. (\E p \in ProcSet : pc[p] \in {"wait2"}) => \E q \in ProcSet : pc[q] \in {"loaded", "wait1", "pause"}
    BY DEF Init
  <2>x. rc = failed <=> \E p \in ProcSet : pc[p] \in {"load_failed"}
    BY distinct DEF Init
  <2>8. QED
    BY <2>1, <2>2, <2>3, <2>4, <2>5, <2>6, <2>7, <2>x DEF IInv
      
<1>2. IInv /\ [Next]_vars => IInv'
      BY NextOK
<1>3. Init /\ [][Next]_vars => []IInv
      BY <1>1, <1>2, PTL
<1>4. QED
      BY <1>3 DEF Spec

THEOREM NoEnterWaitOK == Spec => [][NoEnterWait]_vars
<1> SUFFICES ASSUME []IInv /\ [][Next]_vars
             PROVE  [][NoEnterWait]_vars
    BY InvOK DEF Spec
<1> IInv /\ [Next]_vars => NoEnterWait
  <2> CASE UNCHANGED vars
      BY DEF vars, IInv, NoEnterWait
  <2> SUFFICES ASSUME IInv,
                      <<Next>>_vars,
                      NEW p \in ProcSet,
                      pc[p] # "wait2",
                      pc'[p] = "wait2",
                      NEW q \in ProcSet,
                      pc[q] = "pause"
               PROVE  FALSE
    BY DEF NoEnterWait
  <2>1. \E self \in ProcSet : Proc(self)
        BY DEF Next, ProcSet
  <2>2. start(p)
        BY <2>1 DEF start, loaded, wait1, wait2, pause, release, load_failed, Proc, IInv
  <2>3. rc \notin resolved
        BY <2>2 DEF start
  <2>4. rc \in resolved
        BY pc[q] = "pause", HasUsers
  <2> QED
        BY <2>3, <2>4
<1> QED
    BY PTL


=============================================================================
\* Modification History
\* Last modified Sun Mar 11 17:10:59 GMT 2018 by user
\* Last modified Wed Mar 07 12:10:29 GMT 2018 by tal
\* Created Mon Feb 05 10:28:24 GMT 2018 by tal
