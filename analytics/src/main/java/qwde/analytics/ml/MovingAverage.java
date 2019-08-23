package qwde.analytics.ml;

import java.util.Arrays;

public final class MovingAverage {

    public static Double[] simpleMovingAverage(Double[] data, int time) {

        Double[] sma = new Double[data.length];

        for (int i = 0; i < data.length; i++) {
            
            int window = time;

            if (i < time) {
                window = i + 1;
            }

            Double[] windowData = Arrays.copyOfRange(sma, i - (window - 1), i);
            sma[i] = Arrays.stream(windowData).reduce(0.0, (subtotal, element) -> subtotal + element) / window;
        }

        return sma;
    }
};
