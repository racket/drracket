package edu.rice.cs.drj;

abstract public class SchemeValue {
  protected int ptr;
  static {
    System.loadLibrary("SchemeValue");
    initNative();
  }
  static native void initNative();
}
