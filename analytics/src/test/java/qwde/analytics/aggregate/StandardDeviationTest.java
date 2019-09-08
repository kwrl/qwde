package qwde.analytics.aggregate;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;

class StandardDeviationTest {

  @Test
  void standardDeviation_SingleElement_0() {
    List<Double> data = DoubleStream.of(1.0).boxed().collect(Collectors.toList());
    List<Double> standardDeviation = StandardDeviation.rollingStandardDeviation(data, data.size());

    Truth.assertThat(standardDeviation).isEqualTo(Collections.singletonList(0.0));
  }

  @Test
  void standardDeviation_LinearMonotoneInc_Mean() {
    List<Double> data = IntStream.rangeClosed(1, 5).asDoubleStream().boxed().collect(Collectors.toList());
    List<Double> standardDeviation = StandardDeviation.rollingStandardDeviation(data, 5);

    Truth.assertThat(standardDeviation).isEqualTo(Arrays.asList(null, null, null, null, 1.5811388300841898));
  }

  @Test
  void standardDeviationDirichlet_LinearMonotoneInc_Mean() {
    List<Double> data = IntStream.rangeClosed(1, 5).asDoubleStream().boxed().collect(Collectors.toList());
    List<Double> standardDeviation = StandardDeviation.standardDeviationDirichlet(data, 5);

    Truth.assertThat(standardDeviation).isEqualTo(Arrays.asList(0.0, 0.7071067811865476, 1.0, 1.2909944487358056, 1.5811388300841898));
  }
}
