package qwde.analytics.aggregate;

import java.util.List;
import java.util.stream.Collectors;

import tech.tablesaw.api.DoubleColumn;

public final class Variance {
  private Variance() {
  }

  public static double variance(List<Double> data) {
    return DoubleColumn.create("data", data.stream().toArray(Double[]::new)).variance();
  }

  public static List<Double> rollingVariance(List<Double> data, int windowSize) {
    return ((DoubleColumn) DoubleColumn.create("data", data.stream().toArray(Double[]::new))
        .rolling(windowSize)
        .calc(tech.tablesaw.aggregate.AggregateFunctions.variance)
        ).asList();
  }

  public static List<Double> varianceDirichlet(List<Double> data, int windowSize) {
    DoubleColumn dCol = DoubleColumn.create("data", data.stream().toArray(Double[]::new));

    // Fill up dCol, which has just nulls for all values less than windowSize
    List<Double> variances = rollingVariance(data, windowSize).stream().collect(Collectors.toList());
    for (int i = 0; i < windowSize; i++) {
      variances.set(i, dCol.first(i + 1).variance());
    }

    return variances;
  }
}

