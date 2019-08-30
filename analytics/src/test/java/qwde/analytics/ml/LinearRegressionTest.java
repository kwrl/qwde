package qwde.analytics.ml;

import java.io.File;

import org.junit.jupiter.api.Test;

import com.google.common.truth.Truth;

class LinearRegressionTest {

  @Test
  void test() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File resourcesDirectory = new File(classLoader.getResource("linear-test.arff").getFile());
    Truth.assertThat(resourcesDirectory.exists()).isTrue();
    LinearRegression.process(resourcesDirectory);
  }
}

