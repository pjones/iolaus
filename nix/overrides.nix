lib: self: super: with lib; {
  fused-effects =
    if super ? fused-effects_1_0_0_0
      then super.fused-effects_1_0_0_0
      else super.fused-effects;
}
