package qwde.analytics.ml;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class MovingAverage {

  private MovingAverage() {
  }

  public static Double[] simpleMovingAverage(Double[] data, int time) {
    Double[] sma = new Double[data.length];
    for (int i = 0; i < data.length; i++) {
      int window = time;

      if (i < time) {
        window = i + 1;
      }

      Double[] windowData = Arrays.copyOfRange(data, i - (window - 1), i + 1);
      sma[i] = Arrays.stream(windowData).reduce(0.0, (subtotal, element) -> subtotal + element) / window;
    }

    return sma;
  }

  public static List<Double[]> simpleMovingAverages(Double[] data, int windowSize, int step) {
    List<Double[]> windows = new ArrayList<>();
    for (int window = step; window <= windowSize; window += step) {
      windows.add(simpleMovingAverage(data, window));
    }
    return windows;
  }
};
