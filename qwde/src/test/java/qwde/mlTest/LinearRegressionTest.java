package qwde.mlTest;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.IOException;
import org.apache.commons.io.IOUtils;

import static com.google.common.truth.Truth.assertThat;
import static com.google.common.truth.Truth.assertWithMessage;
//import static com.google.common.truth.Truth8.assertThat; // for assertions on Java 8 types

import qwde.ml.LinearRegression;

import org.junit.jupiter.api.Test;

class LinearRegressionTest {
	@Test
	void test() throws Exception {
		File resourcesDirectory = new File("src/test/resources/qwde.mlTest/POOP");
		//LinearRegression.process(new File("linear-test.arff"));
		assertWithMessage("test").that(1).isAtLeast(9);
	}
}
