package qwde.analytics.aggregate;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;

class VarianceTest {

  @Test
  void variance_SingleElement_0() {
    List<Double> data = DoubleStream.of(1.0).boxed().collect(Collectors.toList());
    List<Double> variance = Variance.rollingVariance(data, data.size());

    Truth.assertThat(variance).isEqualTo(Collections.singletonList(0.0));
  }

  @Test
  void variance_LinearMonotoneInc_Mean() {
    List<Double> data = IntStream.rangeClosed(1, 5).asDoubleStream().boxed().collect(Collectors.toList());
    List<Double> variance = Variance.rollingVariance(data, 5);

    Truth.assertThat(variance).isEqualTo(Arrays.asList(null, null, null, null, 2.5));
  }

  @Test
  void varianceDirichlet_LinearMonotoneInc_Mean() {
    List<Double> data = IntStream.rangeClosed(1, 5).asDoubleStream().boxed().collect(Collectors.toList());
    List<Double> variance = Variance.varianceDirichlet(data, 5);

    Truth.assertThat(variance).isEqualTo(Arrays.asList(0.0, 0.5, 1.0, 1.6666666666666667, 2.5));
  }
}

