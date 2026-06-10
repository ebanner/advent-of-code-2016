parse ← {(⊃ ⍵) (⍎ 1 ↓ ⍵)}
get_instructions ← {toks ← (~⍵ ∊ ', ') ⊆ ⍵ ⋄ parse¨ toks}

get_vectors ← {
    get_offset ← {turn magnitude ← ⍵ ⋄ 1 - 2 × turn = 'L'} ⋄
    get_offsets ← {get_offset¨ ⍵} ⋄
    get_unit_vectors ← {(0 1) (1 0) (0 ¯1) (¯1 0)[⍵]} ⋄
    get_magnitude ← {direction magnitude ← ⍵ ⋄ magnitude} ⋄
    get_magnitudes ← {get_magnitude¨ ⍵} ⋄

    offsets ← get_offsets ⍵ ⋄
    directions ← 1 + 4 | 1 + +\ offsets ⋄
    unit_vectors magnitudes ← (get_unit_vectors directions) (get_magnitudes ⍵) ⋄
    unit_vectors × magnitudes
}

line ← ⍞

instructions ← get_instructions line
vectors ← get_vectors instructions
distance ← +/ | +⌿ ↑ vectors

⎕ ← distance
