/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.m2e.config;

import java.io.File;

/** Configuration of a SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLConfiguration {

	private File input;

	private File output;

	private File binOutput;

	private File testInput;

	private File testOutput;

	private File testBinOutput;

	private String inputCompliance;

	private String outputCompliance;

	private String encoding;

	/** Set the uninitialized field with given configuration.
	 *
	 * @param config the configured values.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	public void setFrom(SARLConfiguration config) {
		if (this.input == null) {
			this.input = config.getInput();
		}
		if (this.output == null) {
			this.output = config.getOutput();
		}
		if (this.binOutput == null) {
			this.binOutput = config.getBinOutput();
		}
		if (this.testInput == null) {
			this.testInput = config.getTestInput();
		}
		if (this.testOutput == null) {
			this.testOutput = config.getTestOutput();
		}
		if (this.testBinOutput == null) {
			this.testBinOutput = config.getTestBinOutput();
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

	/** Replies the input file.
	 *
	 * @return the input
	 */
	public File getInput() {
		return this.input;
	}

	/** Set the input file.
	 *
	 * @param input the input to set
	 */
	public void setInput(File input) {
		this.input = input;
	}

	/** Replies the output file.
	 *
	 * @return the output
	 */
	public File getOutput() {
		return this.output;
	}

	/** Set the output file.
	 *
	 * @param output the output to set
	 */
	public void setOutput(File output) {
		this.output = output;
	}

	/** Replies the binary output file.
	 *
	 * @return the binary output
	 * @since 0.8
	 */
	public File getBinOutput() {
		return this.binOutput;
	}

	/** Set the binary output file.
	 *
	 * @param output the binary output to set
	 * @since 0.8
	 */
	public void setBinOutput(File output) {
		this.binOutput = output;
	}

	/** Replies the input file for tests.
	 *
	 * @return the testInput
	 */
	public File getTestInput() {
		return this.testInput;
	}

	/** Set the input file for tests.
	 *
	 * @param testInput the testInput to set
	 */
	public void setTestInput(File testInput) {
		this.testInput = testInput;
	}

	/** Replies the output file for tests.
	 *
	 * @return the testOutput
	 */
	public File getTestOutput() {
		return this.testOutput;
	}

	/** Set the output file for tests.
	 *
	 * @param testOutput the testOutput to set
	 */
	public void setTestOutput(File testOutput) {
		this.testOutput = testOutput;
	}

	/** Replies the binary output file for tests.
	 *
	 * @return the testOutput
	 * @since 0.8
	 */
	public File getTestBinOutput() {
		return this.testBinOutput;
	}

	/** Set the binary output file for tests.
	 *
	 * @param testOutput the testOutput to set
	 * @since 0.8
	 */
	public void setTestBinOutput(File testOutput) {
		this.testBinOutput = testOutput;
	}

	/** Replies the input's Java compliance.
	 *
	 * @return the inputCompliance
	 */
	public String getInputCompliance() {
		return this.inputCompliance;
	}

	/** Change the input's Java compliance.
	 *
	 * @param inputCompliance the inputCompliance to set
	 */
	public void setInputCompliance(String inputCompliance) {
		this.inputCompliance = inputCompliance;
	}

	/** Replies the output's Java compliance.
	 *
	 * @return the outputCompliance
	 */
	public String getOutputCompliance() {
		return this.outputCompliance;
	}

	/** Change the output's Java compliance.
	 *
	 * @param outputCompliance the outputCompliance to set
	 */
	public void setOutputCompliance(String outputCompliance) {
		this.outputCompliance = outputCompliance;
	}

	/** Replies the encoding of the files.
	 *
	 * @return the encoding
	 */
	public String getEncoding() {
		return this.encoding;
	}

	/** Change the encoding of the files.
	 *
	 * @param encoding the encoding to set
	 */
	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

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
