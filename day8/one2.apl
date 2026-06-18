instrs←('rect' 3 2)('rotate-column' 2 1)('rotate-row' 1 4)('rotate-column' 2 1)
grid←3 7⍴0
apply←{
  word _ _←⍵ ⋄
  word≡'rect':{_ w h←⍵ ⋄ grid[⍳h;⍳w]←1 ⋄ grid}⍵ ⋄
  word≡'rotate-column':{_ x mag←⍵ ⋄ grid[;x]←(-mag)⊖grid[;x] ⋄ grid}⍵ ⋄
  word≡'rotate-row':{_ y mag←⍵ ⋄ grid[y;]←(-mag)⌽grid[y;] ⋄ grid}⍵ ⋄
}
⎕←+/+/⊃{apply⍺}/⌽0,instrs
