package qwde.analytics.aggregate;

import one.util.streamex.StreamEx;
import tech.tablesaw.aggregate.AggregateFunctions;
import tech.tablesaw.api.DoubleColumn;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public final class Covariance {
    private Covariance() {
    }

    public static double covariance(List<Double> left, List<Double> right) {
        double leftAvg = left.stream().mapToDouble(x -> x).average().getAsDouble();
        double rightAvg = right.stream().mapToDouble(x -> x).average().getAsDouble();

        return StreamEx.of(left).zipWith(StreamEx.of(right))
              .map(l -> Math.pow(leftAvg - leftAvg, 2.0) * Math.pow(rightAvg - l.getValue(), 2.0))
              .mapToDouble(a -> a)
              .sum()
              / left.size();
    }

    public static List<Double> rollingCovariance(List<Double> left, List<Double> right, int windowSize) {
        List<Double> rollingCovariances = IntStream.range(windowSize, left.size()).mapToObj(i -> covariance(left.subList(i - windowSize, i), right.subList(i - windowSize, i))).collect(Collectors.toList());
        List<Double> head = IntStream.of(windowSize).mapToObj(a -> (Double) null).collect(Collectors.toList());

        head.addAll(rollingCovariances);

        return head;
    }

    public static List<Double> varianceDirichlet(List<Double> left, List<Double> right, int windowSize) {
        List<Double> covariances = rollingCovariance(left, right, windowSize).stream().collect(Collectors.toList());
        for (int i = 0; i < windowSize; i++) {
            covariances.set(i, covariance(left.subList(0, i), right.subList(0, i)));
        }

        return covariances;
    }
}

