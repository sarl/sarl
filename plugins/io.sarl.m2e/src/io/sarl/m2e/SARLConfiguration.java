/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.m2e;

import java.io.File;

/** Configuration of a SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class SARLConfiguration {

	private File input;
	private File output;
	private File testInput;
	private File testOutput;
	private String inputCompliance;
	private String outputCompliance;
	private String encoding;

	/** Set the uninitialized field with given configuration.
	 *
	 * @param config - the configured values.
	 */
	public void setFrom(SARLConfiguration config) {
		if (this.input == null) {
			this.input = config.getInput();
		}
		if (this.output == null) {
			this.output = config.getOutput();
		}
		if (this.testInput == null) {
			this.testInput = config.getTestInput();
		}
		if (this.testOutput == null) {
			this.testOutput = config.getTestOutput();
		}
		if (this.inputCompliance == null) {
			this.inputCompliance = config.getInputCompliance();
		}
		if (this.outputCompliance == null) {
			this.outputCompliance = config.getOutputCompliance();
		}
		if (this.encoding == null) {
			this.encoding = config.getEncoding();
		}
	}

	/**
	 * @return the input
	 */
	public File getInput() {
		return this.input;
	}

	/**
	 * @param input the input to set
	 */
	public void setInput(File input) {
		this.input = input;
	}

	/**
	 * @return the output
	 */
	public File getOutput() {
		return this.output;
	}

	/**
	 * @param output the output to set
	 */
	public void setOutput(File output) {
		this.output = output;
	}

	/**
	 * @return the testInput
	 */
	public File getTestInput() {
		return this.testInput;
	}

	/**
	 * @param testInput the testInput to set
	 */
	public void setTestInput(File testInput) {
		this.testInput = testInput;
	}

	/**
	 * @return the testOutput
	 */
	public File getTestOutput() {
		return this.testOutput;
	}

	/**
	 * @param testOutput the testOutput to set
	 */
	public void setTestOutput(File testOutput) {
		this.testOutput = testOutput;
	}

	/**
	 * @return the inputCompliance
	 */
	public String getInputCompliance() {
		return this.inputCompliance;
	}

	/**
	 * @param inputCompliance the inputCompliance to set
	 */
	public void setInputCompliance(String inputCompliance) {
		this.inputCompliance = inputCompliance;
	}

	/**
	 * @return the outputCompliance
	 */
	public String getOutputCompliance() {
		return this.outputCompliance;
	}

	/**
	 * @param outputCompliance the outputCompliance to set
	 */
	public void setOutputCompliance(String outputCompliance) {
		this.outputCompliance = outputCompliance;
	}

	/**
	 * @return the encoding
	 */
	public String getEncoding() {
		return this.encoding;
	}

	/**
	 * @param encoding the encoding to set
	 */
	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

	/** {@inheritDoc}
	 */
	@Override
	public String toString() {
		return "input = " + this.input //$NON-NLS-1$
			+ "\noutput = " + this.output //$NON-NLS-1$
			+ "\ntestInput = " + this.testInput //$NON-NLS-1$
			+ "\ntestOutput = " + this.testOutput //$NON-NLS-1$
			+ "\ninputCompliance = " + this.inputCompliance //$NON-NLS-1$
			+ "\noutputCompliance = " + this.outputCompliance //$NON-NLS-1$
			+ "\nencoding = " + this.encoding; //$NON-NLS-1$
	}

}
