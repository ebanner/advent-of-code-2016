parse←{(⊃⍵)(⍎1↓⍵)}
get_instrs←{toks←(~⍵∊', ')⊆⍵ ⋄ parse¨toks}

get_vecs ← {
  get_offset←{turn mag←⍵ ⋄ 1-2×turn='L'} ⋄
  get_offsets←{get_offset¨⍵} ⋄
  get_unit_vecs←{(0 1)(1 0)(0 ¯1)(¯1 0)[⍵]} ⋄
  get_mag←{direction mag←⍵ ⋄ mag} ⋄
  get_mags←{get_mag¨⍵} ⋄

  offsets←get_offsets ⍵ ⋄
  dirs←1+4|1++\offsets ⋄
  unit_vecs mags←(get_unit_vecs dirs)(get_mags instrs) ⋄
  unit_vecs×mags
}

line←⍞
instrs←get_instrs line
vecs←get_vecs instrs
dist←+/|+⌿↑vecs
⎕←dist
