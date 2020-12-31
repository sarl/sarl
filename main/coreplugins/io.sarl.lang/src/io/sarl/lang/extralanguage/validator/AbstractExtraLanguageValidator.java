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

package io.sarl.lang.extralanguage.validator;

import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.util.Exceptions;
import org.eclipse.xtext.util.SimpleCache;
import org.eclipse.xtext.validation.AbstractDeclarativeValidator.StateAccess;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure3;

import io.sarl.lang.extralanguage.compiler.ExtraLanguageFeatureNameConverter;
import io.sarl.lang.extralanguage.compiler.ExtraLanguageFeatureNameConverter.ConversionType;
import io.sarl.lang.extralanguage.compiler.ExtraLanguageTypeConverter;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageConversionInitializer;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorContext;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageKeywordProvider;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IssueCodes;

/** The abstract implementation of a validator for an extra target language.
 *
 * <p>This abstract implementation is not a EValidator implementation because the Xtext EValidator infrastructure
 * has issues with error reporting when the validator is dynamically added (as it is in the support class).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractExtraLanguageValidator {

	private static final String CHECKED_FEATURE_CALLS = "io.sarl.lang.validation.extra.CheckedFeatureCalls"; //$NON-NLS-1$

	private ExtraLanguageTypeConverter typeConverter;

	private ExtraLanguageFeatureNameConverter featureConverter;

	private Injector injector;

	private final ThreadLocal<Context> currentContext = new ThreadLocal<>();

	private volatile Set<MethodWrapper> checkMethods;

	private final SimpleCache<Class<?>, List<MethodWrapper>> methodsForType = new SimpleCache<>(it -> updateMethodCache(it));

	private final IExtraLanguageKeywordProvider keywords;

	/** Constructor.
	 *
	 * @param keywordProvider the provider of extra-language keywords.
	 * @since 0.8
	 */
	public AbstractExtraLanguageValidator(IExtraLanguageKeywordProvider keywordProvider) {
		assert keywordProvider != null;
		this.keywords = keywordProvider;
	}

	/** Replies the provider of the extra-language keywords.
	 *
	 * @return the provider.
	 * @since 0.8
	 */
	public IExtraLanguageKeywordProvider getExtraLanguageKeywordProvider() {
		return this.keywords;
	}

	private List<MethodWrapper> updateMethodCache(Class<?> parameterType) {
		final List<MethodWrapper> result = new ArrayList<>();
		for (final MethodWrapper method : AbstractExtraLanguageValidator.this.checkMethods) {
			if (method.isMatching(parameterType)) {
				result.add(method);
			}
		}
		return result;
	}

	/** Change the injector.
	 *
	 * @param injector the injector.
	 */
	@Inject
	public void setInjector(Injector injector) {
		this.injector = injector;
	}

	/** Replies the current context.
	 *
	 * @return the current context.
	 */
	protected Context getContext() {
		return this.currentContext.get();
	}

	/** Initialize the given context. This function in invoked when the validator is called with a resource and
	 * before the validation on the resource's content is started.
	 *
	 * @param validatorContext the context to initialize.
	 * @since 0.8
	 */
	protected void initializeContext(Context validatorContext) {
		//
	}

	/** Validate the given resource.
	 *
	 * @param validationState the current validation state.
	 * @param messageAcceptor the message acceptor.
	 */
	public void validate(StateAccess validationState, ValidationMessageAcceptor messageAcceptor) {
		if (isResponsible(validationState.getState().context, validationState.getState().currentObject)) {
			try {
				for (final MethodWrapper method : getMethods(validationState.getState().currentObject)) {
					final Context ctx = new Context(validationState, this, method, messageAcceptor);
					this.currentContext.set(ctx);
					initializeContext(ctx);
					method.invoke(ctx);
				}
			} finally {
				this.currentContext.set(null);
			}
		}
	}

	/** Collect the check methods.
	 *
	 * @param clazz the type to explore.
	 * @param visitedClasses the visited classes.
	 * @param result the collected methods.
	 */
	protected void collectMethods(Class<? extends AbstractExtraLanguageValidator> clazz,
			Collection<Class<?>> visitedClasses,
			Collection<MethodWrapper> result) {
		if (!visitedClasses.add(clazz)) {
			return;
		}
		for (final Method method : clazz.getDeclaredMethods()) {
			if (method.getAnnotation(Check.class) != null && method.getParameterTypes().length == 1) {
				result.add(createMethodWrapper(method));
			}
		}
		final Class<? extends AbstractExtraLanguageValidator> superClass = getSuperClass(clazz);
		if (superClass != null) {
			collectMethods(superClass, visitedClasses, result);
		}
	}

	/** Create the method wrapper.
	 *
	 * @param method the wrapped method.
	 * @return the wrapper.
	 */
	@SuppressWarnings("static-method")
	protected MethodWrapper createMethodWrapper(Method method) {
		return new MethodWrapper(method);
	}

	private static Class<? extends AbstractExtraLanguageValidator> getSuperClass(
			Class<? extends AbstractExtraLanguageValidator> clazz) {
		try {
			final Class<? extends AbstractExtraLanguageValidator> superClass = clazz.getSuperclass().asSubclass(
					AbstractExtraLanguageValidator.class);
			if (AbstractExtraLanguageValidator.class.equals(superClass)) {
				return null;
			}
			return superClass;
		} catch (ClassCastException exception) {
			return null;
		}
	}

	private Iterable<MethodWrapper> getMethods(EObject currentObject) {
		if (currentObject == null) {
			return Collections.emptyList();
		}
		if (this.checkMethods == null) {
			synchronized (this) {
				if (this.checkMethods == null) {
					final Set<MethodWrapper> checkMethods = new LinkedHashSet<>();
					final Set<Class<?>> visitedClasses = new HashSet<>(4);
					collectMethods(getClass(), visitedClasses, checkMethods);
					this.checkMethods = checkMethods;
				}
			}
		}
		return this.methodsForType.get(currentObject.getClass());
	}

	/** Replies if the validator is responsible to validate the given object.
	 *
	 * @param context the context.
	 * @param eObject the validated object.
	 * @return {@code true} if the validator could be run.
	 */
	@SuppressWarnings("static-method")
	protected boolean isResponsible(Map<Object, Object> context, EObject eObject) {
		// Skip the validation of an feature call if one of its container was validated previously
		if (eObject instanceof XMemberFeatureCall || eObject instanceof XFeatureCall) {
			final XAbstractFeatureCall rootFeatureCall = Utils.getRootFeatureCall((XAbstractFeatureCall) eObject);
			return !isCheckedFeatureCall(context, rootFeatureCall);
		}
		return true;
	}

	/** Generate an error for the extra-language.
	 *
	 * <p>This function generates an error, a warning, or an information depending on the extra-language generation's
	 * configuration.
	 *
	 * @param message the error message.
	 * @param source the source of the error.
	 * @param feature the structural feature.
	 */
	protected void error(String message, EObject source, EStructuralFeature feature) {
		getContext().getMessageAcceptor().acceptError(
				MessageFormat.format(getErrorMessageFormat(), message),
				source,
				feature,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				IssueCodes.INVALID_EXTRA_LANGUAGE_GENERATION);
	}

	/** Generate a warning for the extra-language.
	 *
	 * <p>This function generates an error, a warning, or an information depending on the extra-language generation's
	 * configuration.
	 *
	 * @param message the warning message.
	 * @param source the source of the error.
	 * @param feature the structural feature.
	 */
	protected void warning(String message, EObject source, EStructuralFeature feature) {
		getContext().getMessageAcceptor().acceptWarning(
				MessageFormat.format(getErrorMessageFormat(), message),
				source,
				feature,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				IssueCodes.INVALID_EXTRA_LANGUAGE_GENERATION);
	}

	/** Generate an information message for the extra-language.
	 *
	 * <p>This function generates an error, a warning, or an information depending on the extra-language generation's
	 * configuration.
	 *
	 * @param message the info message.
	 * @param source the source of the error.
	 * @param feature the structural feature.
	 */
	protected void info(String message, EObject source, EStructuralFeature feature) {
		getContext().getMessageAcceptor().acceptInfo(
				MessageFormat.format(getErrorMessageFormat(), message),
				source,
				feature,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				IssueCodes.INVALID_EXTRA_LANGUAGE_GENERATION);
	}

	/** Replies the message format to be used for building an alert message.
	 *
	 * <p>The replied format must be compatible with {@link MessageFormat#format(Object)} with the
	 * first argument <code>{0}</code> equals to the text of the alert message.
	 *
	 * @return the alert message format.
	 */
	protected abstract String getErrorMessageFormat();

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

	/** Replies the type converter.
	 *
	 * @return the type converter.
	 */
	public ExtraLanguageTypeConverter getTypeConverter() {
		ExtraLanguageTypeConverter converter = this.typeConverter;
		if (converter == null) {
			converter = createTypeConverterInstance(getTypeConverterInitializer(), null);
			this.injector.injectMembers(converter);
			this.typeConverter = converter;
		}
		return converter;
	}

	/** Create the instance of the type converter.
	 *
	 * @param initializer the converter initializer.
	 * @param context the generation context.
	 * @return the type converter.
	 */
	@SuppressWarnings("static-method")
	protected ExtraLanguageTypeConverter createTypeConverterInstance(
			IExtraLanguageConversionInitializer initializer,
			IExtraLanguageGeneratorContext context) {
		return new ExtraLanguageTypeConverter(initializer, context);
	}

	/** Replies the feature name converter.
	 *
	 * @return the feature name converter.
	 */
	public ExtraLanguageFeatureNameConverter getFeatureNameConverter() {
		ExtraLanguageFeatureNameConverter converter = this.featureConverter;
		if (converter == null) {
			converter = createFeatureNameConverterInstance(getFeatureConverterInitializer(), null);
			this.injector.injectMembers(converter);
			this.featureConverter = converter;
		}
		return converter;
	}

	/** Create the instance of the feature name converter.
	 *
	 * @param initializer the converter initializer.
	 * @param context the generation context.
	 * @return the feature name converter.
	 */
	protected ExtraLanguageFeatureNameConverter createFeatureNameConverterInstance(
			IExtraLanguageConversionInitializer initializer,
			IExtraLanguageGeneratorContext context) {
		return new ExtraLanguageFeatureNameConverter(initializer, context, getExtraLanguageKeywordProvider());
	}

	/** Do a type mapping check.
	 *
	 * @param source the source of the type.
	 * @param type the type to check.
	 * @param errorHandler the error handler.
	 * @return {@code true} if a type mapping is defined.
	 */
	protected boolean doTypeMappingCheck(EObject source, JvmType type, Procedure3<? super EObject, ? super JvmType, ? super String> errorHandler) {
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
			Procedure3<? super EObject, ? super JvmType, ? super String> typeErrorHandler,
			Function2<? super EObject, ? super JvmIdentifiableElement, ? extends Boolean> featureErrorHandler) {
		final XAbstractFeatureCall rootFeatureCall = Utils.getRootFeatureCall(featureCall);
		final Map<Object, Object> context = getContext().getContext();
		if (isCheckedFeatureCall(context, rootFeatureCall)) {
			// One of the containing expressions was already checked.
			return;
		}
		// Mark the root container as validated.
		setCheckedFeatureCall(context, rootFeatureCall);
		// Validate the current call.
		internalCheckMemberFeaturCallMapping(rootFeatureCall, typeErrorHandler, featureErrorHandler);
	}

	private boolean internalCheckMemberFeaturCallMapping(XAbstractFeatureCall featureCall,
			Procedure3<? super EObject, ? super JvmType, ? super String> typeErrorHandler,
			Function2<? super EObject, ? super JvmIdentifiableElement, ? extends Boolean> featureErrorHandler) {
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

	/** Handle an exception.
	 *
	 * @param targetException the exception.
	 * @param context the context.
	 */
	@SuppressWarnings("static-method")
	protected void handleInvocationTargetException(Throwable targetException, Context context) {
		// ignore NullPointerException, as not having to check for NPEs all the time is a convenience feature
		if (!(targetException instanceof NullPointerException)) {
			Exceptions.throwUncheckedException(targetException);
		}
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

	/** Validation context.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class Context {

		private final StateAccess validationState;

		private final MethodWrapper currentMethod;

		private final WeakReference<AbstractExtraLanguageValidator> currentValidator;

		private final ValidationMessageAcceptor messageAcceptor;

		/** Constructor.
		 *
		 * @param validationState the validation state.
		 * @param validator the validator instance.
		 * @param currentMethod the current method.
		 * @param messageAcceptor the message acceptor.
		 */
		Context(StateAccess validationState, AbstractExtraLanguageValidator validator,
				MethodWrapper currentMethod, ValidationMessageAcceptor messageAcceptor) {
			this.currentValidator = new WeakReference<>(validator);
			this.validationState = validationState;
			this.currentMethod = currentMethod;
			this.messageAcceptor = messageAcceptor;
		}

		/** Replies the object under validation.
		 *
		 * @return the object under validation.
		 */
		public EObject getCurrentObject() {
			return this.validationState.getState().currentObject;
		}

		/** Replies the current validation method.
		 *
		 * @return the current validation method.
		 */
		public Method getCurrentMethod() {
			return this.currentMethod.getMethod();
		}

		/** Replies the current validator.
		 *
		 * @return the current validator.
		 */
		public AbstractExtraLanguageValidator getCurrentValidator() {
			return this.currentValidator.get();
		}

		/** Replies the diagnostic chain.
		 *
		 * @return the diagnostic chain.
		 */
		public DiagnosticChain getChain() {
			return this.validationState.getState().chain;
		}

		/** Replies the current check mode.
		 *
		 * @return the mode.
		 */
		public CheckMode getCheckMode() {
			return this.validationState.getState().checkMode;
		}

		/** Replies the current check type.
		 *
		 * @return the type.
		 */
		public CheckType getCheckType() {
			return this.validationState.getState().currentCheckType;
		}

		/** Replies the current validation context.
		 *
		 * @return the context.
		 */
		public Map<Object, Object> getContext() {
			return this.validationState.getState().context;
		}

		/** Replies the message acceptor.
		 *
		 * @return the message acceptor.
		 */
		public ValidationMessageAcceptor getMessageAcceptor() {
			return this.messageAcceptor;
		}

	}

	/** Method wrapper.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class MethodWrapper {

		private final Method method;

		private final CheckType checkType;

		/** Constructor.
		 *
		 * @param method the method.
		 */
		public MethodWrapper(Method method) {
			this.method = method;
			this.method.setAccessible(true);
			this.checkType = this.method.getAnnotation(Check.class).value();
		}

		@Override
		public String toString() {
			return this.method.toString();
		}

		/** Replies the wrapped method.
		 *
		 * @return the wrapped method.
		 */
		public Method getMethod() {
			return this.method;
		}

		/** Replies if this method is matching the parameter.
		 *
		 * @param param the parameter type.
		 * @return {@code true} if the method matches the parameter type.
		 */
		public boolean isMatching(Class<?> param) {
			return this.method.getParameterTypes()[0].isAssignableFrom(param);
		}

		/** Invoke the method.
		 *
		 * @param context the context.
		 */
		public void invoke(Context context) {
			if (!context.getCheckMode().shouldCheck(this.checkType)) {
				return;
			}
			try {
				this.method.invoke(context.getCurrentValidator(), context.getCurrentObject());
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException exception) {
				context.getCurrentValidator().handleInvocationTargetException(exception, context);
			}
		}

	}

}
