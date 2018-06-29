package qwde.ml;

import java.io.File;
import java.io.IOException;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ArffLoader;

/**
 * Stolen from https://tech.io/playgrounds/3771/machine-learning-with-java---part-1-linear-regression
 */
public final class LinearRegression {
  /** File names are defined. */
  public static final String TRAINING_DATA_SET_FILENAME = "linear-train.arff";
  public static final String PREDICTION_DATA_SET_FILENAME = "linear-prediction.arff";

  static Instances getDataSet(File file) throws IOException {
    /**
     * we can set the file i.e., loader.setFile("finename") to load the data
     */
    int classIdx = 1;
    /** the arffloader to load the arff file. */
    ArffLoader loader = new ArffLoader();
    /** load the traing data */
    loader.setSource(file);
    /**
     * we can also set the file like loader3.setFile(new
     * File("test-confused.arff"));
     */
    Instances dataSet = loader.getDataSet();
    /** set the index based on the data given in the arff files */
    dataSet.setClassIndex(classIdx);
    return dataSet;
  }

  public static void process(File dataSetFile) throws Exception {
    ClassLoader classLoader = LinearRegression.class.getClassLoader();
    File trainingDataFile = new File(classLoader.getResource(TRAINING_DATA_SET_FILENAME).getFile());
    assert trainingDataFile.exists();
    Instances trainingDataSet = getDataSet(trainingDataFile);
    Instances dataSet = getDataSet(dataSetFile);
    /** Classifier here is Linear Regression. */
    Classifier classifier = new weka.classifiers.functions.LinearRegression();
    /** */
    classifier.buildClassifier(trainingDataSet);
    /**
     * train the alogorithm with the training data and evaluate the
     * algorithm with testing data.
     */
    Evaluation eval = new Evaluation(trainingDataSet);
    eval.evaluateModel(classifier, dataSet);
    /** Print the algorithm summary */
    System.out.println("** Linear Regression Evaluation with Datasets **");
    System.out.println(eval.toSummaryString());
    System.out.print(" the expression for the input data as per alogorithm is ");
    System.out.println(classifier);

    Instance predicationDataSet = getDataSet(new File(PREDICTION_DATA_SET_FILENAME)).lastInstance();
    double value = classifier.classifyInstance(predicationDataSet);
    /** Prediction Output */
    System.out.println(value);
  }

  private LinearRegression() {
  }
}

