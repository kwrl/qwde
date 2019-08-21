package qwde.ml.test;

import java.io.File;

import com.google.common.truth.Truth;
import org.junit.jupiter.api.Test;

import qwde.ml.LinearRegression;

public class LinearRegressionTest {

  @Test
  public void test() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File resourcesDirectory = new File(classLoader.getResource("linear-test.arff").getFile());
    Truth.assertThat(resourcesDirectory.exists()).isTrue();
    LinearRegression.process(resourcesDirectory);
  }
}

