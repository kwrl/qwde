package qwde.analytics.aggregate;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;

class LinearRegressionTest {

  @Test
  public void standardDeviation_SingleElement_Id() throws Exception {
    List<Double> data = DoubleStream.of(1.0).boxed().collect(Collectors.toList());
    List<Double> standardDeviation = StandardDeviation.standardDeviaton(data, data.size());

    Truth.assertThat(standardDeviation).isEqualTo(Collections.singletonList(1.0));
  }

  @Test
  public void standardDeviation_LinearMonotoneInc_Mean() throws Exception {
    List<Double> data = IntStream.rangeClosed(1, 5).asDoubleStream().boxed().collect(Collectors.toList());
    List<Double> standardDeviation = StandardDeviation.standardDeviaton(data, 5);

    Truth.assertThat(standardDeviation).isEqualTo(Collections.singletonList(1.0));
  }
}
