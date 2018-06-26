package qwde.ml.test;

import java.io.File;
import static com.google.common.truth.Truth.assertThat;
import qwde.ml.LinearRegression;

import org.junit.Test;

public class LinearRegressionTest {
	@Test
	public void test() throws Exception {
		ClassLoader classLoader = getClass().getClassLoader();
		File resourcesDirectory = new File(classLoader.getResource("linear-test.arff").getFile());
		assertThat(resourcesDirectory.exists()).isTrue();
		LinearRegression.process(resourcesDirectory);
	}
}
