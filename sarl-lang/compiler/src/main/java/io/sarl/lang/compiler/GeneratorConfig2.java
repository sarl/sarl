/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.compiler;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.SARLConfig;

/** Configuration for the SARL generator.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 */
public class GeneratorConfig2 {

	/** Default value for the generation of the inline annotation flag.
	 *
	 * @since 0.8
	 */
	public static final boolean DEFAULT_GENERATE_INLINE_ANNOTATION = false;

	/** Default value for the flag that indicates if the expression interpreter should be used when
	 * generating inline annotations.
	 *
	 * @since 0.8
	 */
	public static final boolean DEFAULT_USE_EXPRESSION_INTERPRETER_FOR_INLINE_ANNOTATION = true;

	/** Default value for the generation flag of the pure annotations.
	 *
	 * @since 0.8
	 */
	public static final boolean DEFAULT_GENERATE_PURE_ANNOTATION = true;

	/** Default value for the generation flag of the equality test functions.
	 *
	 * @since 0.8
	 */
	public static final boolean DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS = true;

	/** Default value for the generation flag of the serial number field.
	 *
	 * @since 0.8
	 */
	public static final boolean DEFAULT_GENERATE_SERIAL_NUMBER_FIELD = true;

	/** Default value for the generation flag of the toString function.
	 *
	 * @since 0.8
	 */
	public static final boolean DEFAULT_GENERATE_TOSTRING_FUNCTION = true;

	/** Default value for the generation flag of the clone function.
	 *
	 * @since 0.8
	 */
	public static final boolean DEFAULT_GENERATE_CLONE_FUNCTION = true;

	/**
	 * Whether {@code @Inline} shall be generated.
	 */
	private boolean generateInlineAnnotation = DEFAULT_GENERATE_INLINE_ANNOTATION;

	/**
	 * Whether constant expression interpreter shall be called for generated {@code @Inline}.
	 */
	private boolean useExpressionInterpreterForInlineAnnotation = DEFAULT_USE_EXPRESSION_INTERPRETER_FOR_INLINE_ANNOTATION;

	/**
	 * Whether {@code @Pure} shall be generated.
	 * @since 0.8
	 */
	private boolean generatePureAnnotation = DEFAULT_GENERATE_PURE_ANNOTATION;

	/**
	 * Whether the equality test functions shall be generated.
	 * @since 0.8
	 */
	private boolean generateEqualityTestFunctions = DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS;

	/**
	 * Whether the serial number fields shall be generated.
	 * @since 0.8
	 */
	private boolean generateSerialNumberField = DEFAULT_GENERATE_SERIAL_NUMBER_FIELD;

	/**
	 * Whether the toString function shall be generated.
	 * @since 0.8
	 */
	private boolean generateToStringFunction = DEFAULT_GENERATE_TOSTRING_FUNCTION;

	/**
	 * Whether the clone function shall be generated.
	 * @since 0.8
	 */
	private boolean generateCloneFunction = DEFAULT_GENERATE_CLONE_FUNCTION;

	/**
	 * Name of the folder in which the generated test source code is written.
	 * @since 0.8
	 */
	private String generatedTestSourceCodeFolder = SARLConfig.FOLDER_TEST_SOURCE_GENERATED;

	/** Replies if the {@code @Inline} shall be generated.
	 *
	 * @return {@code true} if annotation shall be generated.
	 */
	@Pure
	public boolean isGenerateInlineAnnotation() {
		return this.generateInlineAnnotation;
	}

	/** Set if the {@code @Inline} shall be generated.
	 *
	 * @param generateInlineAnnotation {@code true} if annotation shall be generated.
	 */
	public void setGenerateInlineAnnotation(final boolean generateInlineAnnotation) {
		this.generateInlineAnnotation = generateInlineAnnotation;
	}

	/** Replies if constant expression interpreter shall be called for generated {@code @Inline}.
	 *
	 * @return {@code true} if annotation shall be generated.
	 */
	@Pure
	public boolean isUseExpressionInterpreterForInlineAnnotation() {
		return this.useExpressionInterpreterForInlineAnnotation;
	}

	/** Set if the constant expression interpreter shall be called for generated {@code @Inline}.
	 *
	 * @param generateInlineAnnotation {@code true} if annotation shall be generated.
	 */
	public void setUseExpressionInterpreterForInlineAnnotation(final boolean generateInlineAnnotation) {
		this.useExpressionInterpreterForInlineAnnotation = generateInlineAnnotation;
	}

	/** Replies if the {@code @Pure} shall be generated.
	 *
	 * @return {@code true} if annotation shall be generated.
	 */
	@Pure
	public boolean isGeneratePureAnnotation() {
		return this.generatePureAnnotation;
	}

	/** Set if the {@code @Pure} shall be generated.
	 *
	 * @param generatePureAnnotation {@code true} if annotation shall be generated.
	 */
	public void setGeneratePureAnnotation(final boolean generatePureAnnotation) {
		this.generatePureAnnotation = generatePureAnnotation;
	}

	/** Replies if the equality test functions shall be generated.
	 *
	 * @return {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateEqualityTestFunctions() {
		return this.generateEqualityTestFunctions;
	}

	/** Set if the equality test functions shall be generated.
	 *
	 * @param generateFunctions {@code true} if functions shall be generated.
	 * @since 0.8
	 */
	public void setGenerateEqualityTestFunctions(final boolean generateFunctions) {
		this.generateEqualityTestFunctions = generateFunctions;
	}

	/** Replies if the toString functions shall be generated.
	 *
	 * @return {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateToStringFunctions() {
		return this.generateToStringFunction;
	}

	/** Set if the toString functions shall be generated.
	 *
	 * @param generateFunctions {@code true} if functions shall be generated.
	 * @since 0.8
	 */
	public void setGenerateToStringFunctions(final boolean generateFunctions) {
		this.generateToStringFunction = generateFunctions;
	}

	/** Replies if the clone functions shall be generated.
	 *
	 * @return {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateCloneFunctions() {
		return this.generateCloneFunction;
	}

	/** Set if the clone functions shall be generated.
	 *
	 * @param generateFunctions {@code true} if functions shall be generated.
	 * @since 0.8
	 */
	public void setGenerateCloneFunctions(final boolean generateFunctions) {
		this.generateCloneFunction = generateFunctions;
	}

	/** Replies if the serial number field shall be generated.
	 *
	 * @return {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateSerialNumberFields() {
		return this.generateSerialNumberField;
	}

	/** Set if the clone functions shall be generated.
	 *
	 * @param generateFields {@code true} if fields shall be generated.
	 * @since 0.8
	 */
	public void setGenerateSerialNumberFields(final boolean generateFields) {
		this.generateSerialNumberField = generateFields;
	}

	/** Replies the folder in which the generated test source in written.
	 *
	 * @return the name of the folder.
	 * @since 0.8
	 */
	public String getGeneratedTestSourceFolder() {
		return this.generatedTestSourceCodeFolder;
	}

	/** Set the folder in which the generated test source in written.
	 *
	 * @param folder the name of the folder.
	 * @since 0.8
	 */
	public void setGeneratedTestSourceFolder(String folder) {
		assert folder != null;
		this.generatedTestSourceCodeFolder = folder;
	}

}
