package qwde.analytics.aggregate;

import java.util.List;

import tech.tablesaw.api.DoubleColumn;

public class StandardDeviation {
  public static List<Double> standardDeviaton(List<Double> data, int windowSize) {
    return ((DoubleColumn) DoubleColumn.create("data", data.stream().toArray(Double[]::new))
        .rolling(windowSize)
        .calc(tech.tablesaw.aggregate.AggregateFunctions.standardDeviation)
        ).asList();
  }
}
