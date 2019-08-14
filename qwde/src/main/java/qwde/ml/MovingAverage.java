package qwde.ml;

public final class MovingAverage {

    public static Double[] simpleMovingAverage(Double[] data, int time) {

        Double[] sma = new Double[data.length];

        for (int i = 0; i < data.length; i++) {
            
            int window = time;

            if (i < time) {
                window = i + 1;
            }
            
            double sum = 0;
            for (int j = i; j >= (i - (window -1)); j--) {
                sum += data[j];
            }

            sma[i] = sum / window;
        }

        return sma;
    }
};
