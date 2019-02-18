- Each JudgmentStep must have:
    - type : String  (e.g. "typing","subtyping","well-formed")
    - key: String , a key to differentiate among different step in a derivation tree of a judgment (of certain type)
    - premises: List[Any]. List of premises of the infer rule. Premises can be other judgment (even of other types) or predicates (especial case of judgments, e.g. $x \in Dom(\Gamma)$).
    - conclusion:        
        - context : the things before the $|-$ sign
        - goal : the things after the $|-$ sign
    - solved: whether or not we know if the judgment hold or not. (A judgment step is solve when all its premises have been solve)
    - precedingJudgment: JudgmentStep. The judgment step that generate this.


## About visualization
- By default, we just steps the judgment steps of the same type. It means if we are showing
a typing judgement and it uses a subtyping judgment, the subtyping judgment are not going to be step, they are computed at once. An option will be provided to see the details of secondary judgments

- When a judgment step resolves it must propagates this result to its "preceding" judgment. We will use three colors for judgments states:
    - solved, holds :  green
    - solved, does not hold: red
    - unsolved: gray