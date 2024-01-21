We use dynamic programming with the following recursive formula : 
(see Wikipedia page or [Prologin correction](https://prologin.org/static/archives/2015/questionnaire/correction.pdf)(in french))

```txt
⎧W⁰ᵢⱼ = ω if (i, j, ω) exists, 
⎪      ∞ else
⎨
⎪
⎩Wᵏᵢⱼ = min (Wᵏ⁻¹ᵢⱼ, Wᵏ⁻¹ᵢₖ + Wᵏ⁻¹ₖⱼ)
```
