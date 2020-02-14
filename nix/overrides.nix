lib: self: super: with lib; {
  fused-effects =
    if super ? fused-effects_1_0_0_1
      then super.fused-effects_1_0_0_1
      else super.fused-effects;
}
