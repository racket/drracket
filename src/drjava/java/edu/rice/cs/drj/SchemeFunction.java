package edu.rice.cs.drj;

public final class SchemeFunction extends SchemeValue {
  public native void applyVoid(SchemeList args);
  public native boolean applyBoolean(SchemeList args);
  public native byte applyByte(SchemeList args);
  public native char applyChar(SchemeList args);
  public native short applyShort(SchemeList args);
  public native int applyInt(SchemeList args);
  public native long applyLong(SchemeList args);
  public native float applyFloat(SchemeList args);
  public native double applyDouble(SchemeList args);
  public native Object applyObject(SchemeList args);

  protected SchemeFunction(int f) { ptr = f; }
}
