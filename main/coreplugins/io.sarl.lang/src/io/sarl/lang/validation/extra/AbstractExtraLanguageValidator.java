/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.validation.extra;

import java.util.Collection;
import java.util.Comparator;
import java.util.Map;
import java.util.TreeSet;

import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure3;
import org.eclipse.xtext.xbase.validation.AbstractXbaseValidator;

import io.sarl.lang.generator.extra.ExtraLanguageFeatureNameConverter;
import io.sarl.lang.generator.extra.ExtraLanguageFeatureNameConverter.ConversionType;
import io.sarl.lang.generator.extra.ExtraLanguageTypeConverter;
import io.sarl.lang.generator.extra.IExtraLanguageConversionInitializer;
import io.sarl.lang.generator.extra.IExtraLanguageGeneratorContext;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IssueCodes;

/** The abstract implementation of a validator for an extra target language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractExtraLanguageValidator extends AbstractXbaseValidator {

	private static final String CHECKED_FEATURE_CALLS = "io.sarl.lang.validation.extra.CheckedFeatureCalls"; //$NON-NLS-1$

	private ExtraLanguageTypeConverter typeConverter;

	private ExtraLanguageFeatureNameConverter featureConverter;

	@Inject
	private Injector injector;

	@Override
	public void setInjector(Injector injector) {
		super.setInjector(injector);
		this.injector = injector;
	}

	/** Generate an error for the extra-language.
	 *
	 * <p>This function generates an error, a warning, or an information depending on the extra-language generation's
	 * configuration.
	 *
	 * @param message the error message.
	 * @param source the source of the error.
	 */
	protected void error(String message, EObject source) {
		error(message, source,
				null,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				IssueCodes.INVALID_EXTRA_LANGUAGE_GENERATION);
	}

	/** Replies the initializer for the type converter.
	 *
	 * @return the initializer.
	 */
	protected abstract IExtraLanguageConversionInitializer getTypeConverterInitializer();

	/** Replies the initializer for the feature converter.
	 *
	 * @return the initializer.
	 */
	protected abstract IExtraLanguageConversionInitializer getFeatureConverterInitializer();

	/** Replies the identifier of the generator's plugin.
	 *
	 * @return the plugin's identifier.
	 */
	protected abstract String getGeneratorPluginID();

	/** Replies the type converter.
	 *
	 * @return the type converter.
	 */
	public ExtraLanguageTypeConverter getTypeConverter() {
		ExtraLanguageTypeConverter converter = this.typeConverter;
		if (converter == null) {
			converter = createTypeConverterInstance(getTypeConverterInitializer(), getGeneratorPluginID(), null);
			this.injector.injectMembers(converter);
			this.typeConverter = converter;
		}
		return converter;
	}

	/** Create the instance of the type converter.
	 *
	 * @param initializer the converter initializer.
	 * @param pluginID the identifier of the generator's plugin.
	 * @param context the genetation context.
	 * @return the type converter.
	 */
	@SuppressWarnings("static-method")
	protected ExtraLanguageTypeConverter createTypeConverterInstance(
			IExtraLanguageConversionInitializer initializer,
			String pluginID,
			IExtraLanguageGeneratorContext context) {
		return new ExtraLanguageTypeConverter(initializer, pluginID, context);
	}

	/** Replies the feature name converter.
	 *
	 * @return the feature name converter.
	 */
	public ExtraLanguageFeatureNameConverter getFeatureNameConverter() {
		ExtraLanguageFeatureNameConverter converter = this.featureConverter;
		if (converter == null) {
			converter = createFeatureNameConverterInstance(getFeatureConverterInitializer(), getGeneratorPluginID(), null);
			this.injector.injectMembers(converter);
			this.featureConverter = converter;
		}
		return converter;
	}

	/** Create the instance of the feature name converter.
	 *
	 * @param initializer the converter initializer.
	 * @param pluginID the identifier of the generator's plugin.
	 * @param context the genetation context.
	 * @return the feature name converter.
	 */
	@SuppressWarnings("static-method")
	protected ExtraLanguageFeatureNameConverter createFeatureNameConverterInstance(
			IExtraLanguageConversionInitializer initializer,
			String pluginID,
			IExtraLanguageGeneratorContext context) {
		return new ExtraLanguageFeatureNameConverter(initializer, pluginID, context);
	}

	/** Do a type mapping check.
	 *
	 * @param source the source of the type.
	 * @param type the type to check.
	 * @param errorHandler the error handler.
	 * @return {@code true} if a type mapping is defined.
	 */
	protected boolean doTypeMappingCheck(EObject source, JvmType type, Procedure3<EObject, JvmType, String> errorHandler) {
		if (source != null && type != null) {
			final ExtraLanguageTypeConverter converter = getTypeConverter();
			final String qn = type.getQualifiedName();
			if (converter != null && !converter.hasConversion(qn)) {
				if (errorHandler != null) {
					errorHandler.apply(source, type, qn);
				}
				return false;
			}
		}
		return true;
	}

	@Override
	protected boolean isResponsible(Map<Object, Object> context, EObject eObject) {
		if (!super.isResponsible(context, eObject)) {
			return false;
		}
		// Skip the validation of an feature call if one of its container was validated previously
		if (eObject instanceof XMemberFeatureCall || eObject instanceof XFeatureCall) {
			final XAbstractFeatureCall rootFeatureCall = getRootFeatureCall((XAbstractFeatureCall) eObject);
			return !isCheckedFeatureCall(context, rootFeatureCall);
		}
		return true;
	}

	private static boolean isCheckedFeatureCall(Map<Object, Object> context, EObject eObject) {
		if (eObject instanceof XMemberFeatureCall || eObject instanceof XFeatureCall) {
			final Object calls = context.get(CHECKED_FEATURE_CALLS);
			if (calls != null && ((Collection<?>) calls).contains(eObject)) {
				return true;
			}
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	private static void setCheckedFeatureCall(Map<Object, Object> context, EObject eObject) {
		if (eObject instanceof XMemberFeatureCall || eObject instanceof XFeatureCall) {
			Collection<XAbstractFeatureCall>  calls = (Collection<XAbstractFeatureCall>) context.get(CHECKED_FEATURE_CALLS);
			if (calls == null) {
				calls = new TreeSet<>(FeatureCallComparator.SINGLETON);
				context.put(CHECKED_FEATURE_CALLS, calls);
			}
			calls.add((XAbstractFeatureCall) eObject);
		}
	}

	/** Check if the feature call could be translated to the extra-language.
	 *
	 * @param featureCall the feature call.
	 * @param typeErrorHandler the error handler for the type conversion.
	 * @param featureErrorHandler the error handler for the feature call conversion.
	 */
	protected void doCheckMemberFeatureCallMapping(XAbstractFeatureCall featureCall,
			Procedure3<EObject, JvmType, String> typeErrorHandler,
			Function2<EObject, JvmIdentifiableElement, Boolean> featureErrorHandler) {
		final XAbstractFeatureCall rootFeatureCall = getRootFeatureCall(featureCall);
		final Map<Object, Object> context = getContext();
		if (isCheckedFeatureCall(context, rootFeatureCall)) {
			// One of the containing expressions was already checked.
			return;
		}
		// Mark the root container as validated.
		setCheckedFeatureCall(context, rootFeatureCall);
		// Validate the current call.
		internalCheckMemberFeaturCallMapping(rootFeatureCall, typeErrorHandler, featureErrorHandler);
	}

	private static XAbstractFeatureCall getRootFeatureCall(XAbstractFeatureCall featureCall) {
		final EObject container = featureCall.eContainer();
		final XAbstractFeatureCall rootFeatureCall;
		if (container instanceof XMemberFeatureCall || container instanceof XFeatureCall) {
			rootFeatureCall = (XAbstractFeatureCall) Utils.getFirstContainerForPredicate(featureCall,
				(it) -> it.eContainer() != null && !(it.eContainer() instanceof XMemberFeatureCall || it.eContainer() instanceof XFeatureCall));
		} else {
			rootFeatureCall = featureCall;
		}
		return rootFeatureCall;
	}

	private boolean internalCheckMemberFeaturCallMapping(XAbstractFeatureCall featureCall,
			Procedure3<EObject, JvmType, String> typeErrorHandler,
			Function2<EObject, JvmIdentifiableElement, Boolean> featureErrorHandler) {
		final ExtraLanguageFeatureNameConverter converter = getFeatureNameConverter();
		if (converter != null) {
			final ConversionType conversionType = converter.getConversionTypeFor(featureCall);
			switch (conversionType) {
			case EXPLICIT:
				return true;
			case IMPLICIT:
				final JvmIdentifiableElement element = featureCall.getFeature();
				if (element instanceof JvmType) {
					return doTypeMappingCheck(featureCall, (JvmType) element, typeErrorHandler);
				}
				if (featureCall instanceof XMemberFeatureCall) {
					final XMemberFeatureCall memberFeatureCall = (XMemberFeatureCall) featureCall;
					final XExpression receiver = memberFeatureCall.getMemberCallTarget();
					if (receiver instanceof XMemberFeatureCall || receiver instanceof XFeatureCall) {
						internalCheckMemberFeaturCallMapping(
								(XAbstractFeatureCall) receiver,
								typeErrorHandler,
								featureErrorHandler);
					}
				}
				break;
			case FORBIDDEN_CONVERSION:
			default:
				if (featureErrorHandler != null) {
					return !featureErrorHandler.apply(featureCall, featureCall.getFeature());
				}
				return false;
			}
		}
		return true;
	}

	/** Comparator of feature calls.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private static final class FeatureCallComparator implements Comparator<XAbstractFeatureCall> {

		public static final FeatureCallComparator SINGLETON = new FeatureCallComparator();

		private FeatureCallComparator() {
			//
		}

		@Override
		public int compare(XAbstractFeatureCall o1, XAbstractFeatureCall o2) {
			assert o1 != null && o2 != null;
			if (o1 == o2) {
				return 0;
			}
			return Integer.compare(System.identityHashCode(o1), System.identityHashCode(o2));
		}
	}

}
