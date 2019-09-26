package qwde.analytics.aggregate;

import java.util.List;
import java.util.stream.Collectors;

import tech.tablesaw.api.DoubleColumn;

public final class StandardDeviation {
  private StandardDeviation() {
  }

  public static double standardDeviation(List<Double> data) {
    return DoubleColumn.create("data", data.stream().toArray(Double[]::new)).standardDeviation();
  }

  public static List<Double> rollingStandardDeviation(List<Double> data, int windowSize) {
    return ((DoubleColumn) DoubleColumn.create("data", data.stream().toArray(Double[]::new))
        .rolling(windowSize)
        .calc(tech.tablesaw.aggregate.AggregateFunctions.standardDeviation)
        ).asList();
  }

  public static List<Double> standardDeviationDirichlet(List<Double> data, int windowSize) {
    DoubleColumn dCol = DoubleColumn.create("data", data.stream().toArray(Double[]::new));

    // Fill up dCol, which has just nulls for all values less than windowSize
    List<Double> standardDeviations = rollingStandardDeviation(data, windowSize).stream().collect(Collectors.toList());
    for (int i = 0; i < windowSize; i++) {
      standardDeviations.set(i, dCol.first(i + 1).standardDeviation());
    }

    return standardDeviations;
  }
}
