/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.validator;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilderFactory;

import com.google.common.collect.Iterables;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.jdt.core.Flags;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.SarlSourceCode;
import io.sarl.lang.core.annotation.SyntheticMember;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.util.Utils;

/** Functions that are based on Java reflection, for building the documentation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public final class ReflectExtensions {

	private static final String PLUGIN_HELP_PATH =
			"/META-INF/maven/%s/%s/plugin-help.xml"; //$NON-NLS-1$

	private static Function<Method, String> defaultMethodNameFormatter;

	private static Function<Field, String> defaultFieldNameFormatter;

	private ReflectExtensions() {
		//
	}

	/** Change the default name formatter.
	 *
	 * @param formatter the default name formatter.
	 * @deprecated since 0.12, see {@link #setDefaultMethodNameFormatter(Function)}
	 */
	@Deprecated(forRemoval = true, since = "0.12")
	public static void setDefaultNameFormatter(Function<Method, String> formatter) {
		defaultMethodNameFormatter = formatter;
	}

	/** Change the default name formatter.
	 *
	 * @param formatter the default name formatter.
	 * @since 0.12
	 */
	public static void setDefaultMethodNameFormatter(Function<Method, String> formatter) {
		defaultMethodNameFormatter = formatter;
	}

	/** Replies the default name formatter.
	 *
	 * @return the default name formatter.
	 * @deprecated since 0.12, see {@link #getDefaultMethodNameFormatter()}
	 */
	@Pure
	@Deprecated(forRemoval = true, since = "0.12")
	public static Function<Method, String> getDefaultNameFormatter() {
		return getDefaultMethodNameFormatter();
	}

	/** Replies the default name formatter.
	 *
	 * @return the default name formatter.
	 * @since 0.12
	 */
	@Pure
	public static Function<Method, String> getDefaultMethodNameFormatter() {
		return defaultMethodNameFormatter;
	}

	/** Change the default name formatter.
	 *
	 * @param formatter the default name formatter.
	 * @since 0.12
	 */
	public static void setDefaultFieldNameFormatter(Function<Field, String> formatter) {
		defaultFieldNameFormatter = formatter;
	}

	/** Replies the default field name formatter.
	 *
	 * @return the default field name formatter.
	 * @since 0.12
	 */
	@Pure
	public static Function<Field, String> getDefaultFieldNameFormatter() {
		return defaultFieldNameFormatter;
	}

	private static boolean isDeprecated(Method method) {
		return Flags.isDeprecated(method.getModifiers()) || method.getAnnotation(Deprecated.class) != null;
	}

	private static boolean isDeprecated(Field field) {
		return Flags.isDeprecated(field.getModifiers()) || field.getAnnotation(Deprecated.class) != null;
	}

	/** Extract the public methods from the given types.
	 *
	 * @param type the first type to parse.
	 * @param otherTypes the other types to parse.
	 * @return the code.
	 */
	@Pure
	@Inline(value = "getPublicMethodsWithFormat($1, null, $2)", imported = ReflectExtensions.class)
	public static String getPublicMethods(Class<?> type, Class<?>... otherTypes) {
		return getPublicMethodsWithFormat(type, null);
	}

	/** Extract the public methods from the given types.
	 *
	 * @param type the first type to parse.
	 * @param nameFormatter the formatter for the function's names. If {@code null}, no formatting is applied.
	 * @param otherTypes the other types to parse.
	 * @return the code.
	 */
	@Pure
	public static String getPublicMethodsWithFormat(Class<?> type, Function<Method, String> nameFormatter, Class<?>... otherTypes) {
		final var it = new StringBuilder();
		appendPublicMethods(it, false, nameFormatter, IterableExtensions.flatten(
				Arrays.asList(
						Collections.singletonList(type),
						Arrays.asList(otherTypes))));
		return it.toString();
	}

	/** Extract the public methods from the given types.
	 *
	 * @param it the output.
	 * @param indent indicates if the code should be indented.
	 * @param types the types to parse.
	 */
	@Inline(value = "appendPublicMethods($1, $2, null, $4.asList($3))", imported = Arrays.class)
	public static void appendPublicMethods(StringBuilder it, boolean indent, Class<?>... types) {
		appendPublicMethods(it, indent, null, Arrays.asList(types));
	}

	/** Extract the public methods from the given types.
	 *
	 * @param it the output.
	 * @param indent indicates if the code should be indented.
	 * @param nameFormatter the formatter for the function's names. If {@code null}, no formatting is applied.
	 * @param types the types to parse.
	 */
	public static void appendPublicMethods(StringBuilder it, boolean indent, Function<Method, String> nameFormatter, 
			Iterable<? extends Class<?>> types) {
		appendMethods(it, indent, nameFormatter, types, (it0) -> {
			return Flags.isPublic(it0.getModifiers()) && !SarlUtils.isHiddenMember(it0.getName())
				&& !isDeprecated(it0) && !it0.isSynthetic()
				&& it0.getAnnotation(SyntheticMember.class) == null;
		});
	}

	/** Extract the methods from the given types.
	 *
	 * @param it the output.
	 * @param indent indicates if the code should be indented.
	 * @param nameFormatter the formatter for the function's names. If {@code null}, no formatting is applied.
	 * @param types the types to parse.
	 * @param selector the selector of method.
	 */
	private static void appendMethods(StringBuilder it, boolean indent, Function<Method, String> nameFormatter, 
			Iterable<? extends Class<?>> types, Predicate<Method> selector) {
		final var lines = new LinkedList<String>();
		for (final var type : types) {
			for (final var method : type.getDeclaredMethods()) {
				if (selector.test(method)) {
					final var line = new StringBuilder();
					if (indent) {
						line.append("\t"); //$NON-NLS-1$
					}
					var nformatter = nameFormatter;
					if (nformatter == null) {
						nformatter = getDefaultMethodNameFormatter();
					}
					final String formattedName;
					if (nformatter != null) {
						formattedName = nformatter.apply(method);
					} else {
						formattedName = method.getName();
					}
					line.append("def ").append(formattedName); //$NON-NLS-1$
					if (method.getParameterCount() > 0) {
						line.append("("); //$NON-NLS-1$
						var first = true;
						var i = 1;
						for (final var param : method.getParameters()) {
							if (first) {
								first = false;
							} else {
								line.append(", "); //$NON-NLS-1$
							}
							//it.append(param.getName());
							//it.append(" : "); //$NON-NLS-1$
							toType(line, param.getParameterizedType(), method.isVarArgs() && i == method.getParameterCount());
							final var defaultValue = extractDefaultValueString(param);
							if (!Strings.isEmpty(defaultValue)) {
								line.append(" = "); //$NON-NLS-1$
								line.append(defaultValue);
							}
						}
						line.append(")"); //$NON-NLS-1$
						++i;
					}
					if (method.getGenericReturnType() != null && !Objects.equals(method.getGenericReturnType(), Void.class)
							&& !Objects.equals(method.getGenericReturnType(), void.class)) {
						line.append(" : "); //$NON-NLS-1$
						toType(line, method.getGenericReturnType(), false);
					}
					var first = true;
					for (final var typeVariable : method.getTypeParameters()) {
						if (first) {
							line.append(" with "); //$NON-NLS-1$
							first = false;
						} else {
							line.append(", "); //$NON-NLS-1$
						}
						line.append(typeVariable.getTypeName());
					}
					line.append("\n"); //$NON-NLS-1$
					lines.add(line.toString());
				}
			}
		}
		lines.sort(null);
		for (final var line : lines) {
			it.append(line);
		}
	}

	private static String extractDefaultValueString(Parameter parameter) {
		final var defaultValueAnnotation = parameter.getAnnotation(DefaultValue.class);
		if (defaultValueAnnotation == null) {
			return null;
		}
		final var methodId = defaultValueAnnotation.value();
		if (!Strings.isEmpty(methodId)) {
			final var container = parameter.getDeclaringExecutable().getDeclaringClass();
			if (container != null) {
				final var index = methodId.indexOf('#');
				Class<?> target;
				final String methodName;
				if (index > 0) {
					try {
						final var type = Class.forName(methodId.substring(0, index), true, container.getClassLoader());
						target = type;
					} catch (Throwable exception) {
						target = container;
					}
					methodName = Utils.createNameForHiddenDefaultValueFunction(methodId.substring(index + 1));
				} else {
					target = container;
					methodName = Utils.createNameForHiddenDefaultValueFunction(methodId);
				}

				final var method = Iterables.find(Arrays.asList(target.getDeclaredMethods()),
						(it) -> Strings.equal(it.getName(), methodName),
						null);
				if (method != null) {
					final var sourceCodeAnnotation = parameter.getAnnotation(SarlSourceCode.class);
					if (sourceCodeAnnotation != null) {
						final var value = sourceCodeAnnotation.value();
						if (!Strings.isEmpty(methodId)) {
							return value;
						}
					}
				}
			}
		}
		return null;
	}

	/** Extract the public fields from the given types.
	 *
	 * @param it the output.
	 * @param indent indicates if the code should be indented.
	 * @param types the types to parse.
	 * @since 0.12
	 */
	@Inline(value = "appendPublicFields($1, $2, null, $4.asList($3))", imported = Arrays.class)
	public static void appendPublicFields(StringBuilder it, boolean indent, Class<?>... types) {
		appendPublicFields(it, indent, null, Arrays.asList(types));
	}

	/** Extract the public fields from the given types.
	 *
	 * @param it the output.
	 * @param indent indicates if the code should be indented.
	 * @param nameFormatter the formatter for the field's names. If {@code null}, no formatting is applied.
	 * @param types the types to parse.
	 * @since 0.12
	 */
	public static void appendPublicFields(StringBuilder it, boolean indent, Function<Field, String> nameFormatter, 
			Iterable<? extends Class<?>> types) {
		appendFields(it, indent, nameFormatter, types, (it0) -> {
			return Flags.isPublic(it0.getModifiers()) && !SarlUtils.isHiddenMember(it0.getName())
				&& !isDeprecated(it0) && !it0.isSynthetic()
				&& it0.getAnnotation(SyntheticMember.class) == null;
		});
	}

	/** Extract the fields from the given types.
	 *
	 * @param it the output.
	 * @param indent indicates if the code should be indented.
	 * @param nameFormatter the formatter for the field's names. If {@code null}, no formatting is applied.
	 * @param types the types to parse.
	 * @param selector the selector of field.
	 */
	private static void appendFields(StringBuilder it, boolean indent, Function<Field, String> nameFormatter, 
			Iterable<? extends Class<?>> types, Predicate<Field> selector) {
		final var lines = new LinkedList<String>();
		for (final var type : types) {
			for (final var field : type.getDeclaredFields()) {
				if (selector.test(field)) {
					final var line = new StringBuilder();
					if (indent) {
						line.append("\t"); //$NON-NLS-1$
					}
					var nformatter = nameFormatter;
					if (nformatter == null) {
						nformatter = getDefaultFieldNameFormatter();
					}
					final String formattedName;
					if (nformatter != null) {
						formattedName = nformatter.apply(field);
					} else {
						formattedName = field.getName();
					}
					if (Modifier.isFinal(field.getModifiers())) {
						line.append("val "); //$NON-NLS-1$
					} else {
						line.append("var "); //$NON-NLS-1$
					}
					line.append(formattedName);
					if (field.getGenericType() != null && !Objects.equals(field.getGenericType(), Void.class)
							&& !Objects.equals(field.getGenericType(), void.class)) {
						line.append(" : "); //$NON-NLS-1$
						toType(line, field.getGenericType(), false);
					}
					line.append("\n"); //$NON-NLS-1$
					lines.add(line.toString());
				}
			}
		}
		lines.sort(null);
		for (final var line : lines) {
			it.append(line);
		}
	}

	/** Extract the type name.
	 *
	 * @param it the output.
	 * @param otype the type to parse.
	 * @param isVarArg indicates if the type is used within a variadic parameter.
	 */
	public static void toType(StringBuilder it, Type otype, boolean isVarArg) {
		final Type type;
		if (otype instanceof Class<?> cvalue) {
			type = isVarArg ? cvalue.getComponentType() : otype;
		} else {
			type = otype;
		}
		if (type instanceof Class<?> cvalue) {
			it.append(cvalue.getSimpleName());
		} else if (type instanceof ParameterizedType paramType) {
			final var ownerType = paramType.getOwnerType();
			final var isForFunction = ownerType != null && Functions.class.getName().equals(ownerType.getTypeName());
			final var isForProcedure = ownerType != null && Procedures.class.getName().equals(ownerType.getTypeName());
			if (!isForFunction && !isForProcedure) {
				it.append(((Class<?>) paramType.getRawType()).getSimpleName());
				if (paramType.getActualTypeArguments().length > 0) {
					it.append("<"); //$NON-NLS-1$
					var first = true;
					for (final var subtype : paramType.getActualTypeArguments()) {
						if (first) {
							first = false;
						} else {
							it.append(", "); //$NON-NLS-1$
						}
						final var it2 = new StringBuilder();
						toType(it2, subtype, false);
						it.append(it2);
					}
					it.append(">"); //$NON-NLS-1$
				}
			} else {
				var nb = paramType.getActualTypeArguments().length;
				if (isForFunction) {
					--nb;
				}
				it.append("("); //$NON-NLS-1$
				for (var i = 0; i < nb; ++i) {
					final var subtype = paramType.getActualTypeArguments()[i];
					if (i > 0) {
						it.append(", "); //$NON-NLS-1$
					}
					toType(it, subtype, false);
				}
				it.append(") => "); //$NON-NLS-1$
				if (isForFunction) {
					toType(it, paramType.getActualTypeArguments()[nb], false);
				} else {
					it.append("void"); //$NON-NLS-1$
				}
			}
		} else if (type instanceof WildcardType cvalue) {
			final var types = cvalue.getUpperBounds();
			toType(it, types[0], false);
		} else if (type instanceof GenericArrayType cvalue) {
			toType(it, cvalue.getGenericComponentType(), false);
			it.append("[]"); //$NON-NLS-1$
		} else if (type instanceof TypeVariable<?> cvalue) {
			it.append(cvalue.getName());
		} else {
			it.append(Object.class.getSimpleName());
		}
		if (isVarArg) {
			it.append("*"); //$NON-NLS-1$
		}
	}

	/** Replies the configuration description for a maven plugin.
	 *
	 * @param mavenPluginGroupId the group id of the Maven plugin.
	 * @param mavenPluginArtifactId the artifact id of the Maven plugin.
	 * @return the configuration.
	 * @since 0.8
	 */
	@Pure
	@Inline(
			value = "getMavenPluginConfiguration($1, $2, $3.class)",
			imported = ReflectExtensions.class)
	public static List<List<String>> getMavenPluginConfiguration(String mavenPluginGroupId, String mavenPluginArtifactId) {
		return getMavenPluginConfiguration(mavenPluginGroupId, mavenPluginArtifactId, ReflectExtensions.class);
	}

	/** Replies the configuration description for a maven plugin.
	 *
	 * @param mavenPluginGroupId the group id of the Maven plugin.
	 * @param mavenPluginArtifactId the artifact id of the Maven plugin.
	 * @param classContext the context in which the help resource may be found.
	 * @return the configuration.
	 * @since 0.8
	 */
	@Pure
	public static List<List<String>> getMavenPluginConfiguration(String mavenPluginGroupId, String mavenPluginArtifactId, Class<?> classContext) {
		assert mavenPluginGroupId != null;
		assert mavenPluginArtifactId != null;
		assert classContext != null;
		final var resourceName = String.format(PLUGIN_HELP_PATH, mavenPluginGroupId, mavenPluginArtifactId);
		URL url = classContext.getResource(resourceName);
		if (url == null) {
			// Try to find the plugin's help description as a global resource
			url = ClassLoader.getSystemClassLoader().getResource(resourceName);
			if (url == null) {
				// Try to find the plugin's help description as a local file
				try {
					var userFile = FileSystem.join(FileSystem.getUserHomeDirectory(),
							".m2", "repository"); //$NON-NLS-1$ //$NON-NLS-2$
					for (final var element : mavenPluginGroupId.split(Pattern.quote("."))) { //$NON-NLS-1$
						userFile = new File(userFile, element);
					}
					userFile = FileSystem.join(userFile,
							mavenPluginArtifactId,
							SARLVersion.SARL_RELEASE_VERSION_MAVEN,
							mavenPluginArtifactId + "-" + SARLVersion.SARL_RELEASE_VERSION_MAVEN + ".jar"); //$NON-NLS-1$ //$NON-NLS-2$
					url = FileSystem.toJarURL(userFile, resourceName);
				} catch (Throwable exception) {
					//
				}
			}
		}
		if (url != null) {
			try (var is = url.openStream()) {
				final var dbFactory = DocumentBuilderFactory.newInstance();
				final var dBuilder = dbFactory.newDocumentBuilder();
				final var document = dBuilder.parse(is);
				final var pluginNode = getSingleChild(document, "plugin"); //$NON-NLS-1$
				final var mojosNode = getSingleChild(pluginNode, "mojos"); //$NON-NLS-1$

				final var documentation = new TreeMap<String, List<String>>();

				for (final var mojoNode : findNamedChild(mojosNode, "mojo")) { //$NON-NLS-1$
					final var mojoName = getValue(mojoNode, "goal"); //$NON-NLS-1$
					final var parametersNode = getSingleChild(mojoNode, "parameters"); //$NON-NLS-1$
					final var configurationNode = getSingleChild(mojoNode, "configuration"); //$NON-NLS-1$
					for (final var parameterNode : findNamedChild(parametersNode, "parameter")) { //$NON-NLS-1$
						if (Strings.isEmpty(getValue(parameterNode, "deprecated")) //$NON-NLS-1$
								&& getBoolean(parameterNode, "editable")) { //$NON-NLS-1$
							final var name = getValue(parameterNode, "name"); //$NON-NLS-1$
							var columns = documentation.get(name);
							if (columns == null) {
								columns = new ArrayList<>();
								documentation.put(name, columns);
								columns.add(name);
								columns.add(mojoName);
								columns.add(nullIfEmpty(getValue(parameterNode, "type"))); //$NON-NLS-1$
								columns.add(nullIfEmpty(getValue(parameterNode, "description"))); //$NON-NLS-1$
								columns.add(nullIfEmpty(getDefaultValue(configurationNode, name)));
							} else {
								columns.set(1, columns.get(1) + ", " + mojoName); //$NON-NLS-1$
							}
						}
					}
				}

				return new ArrayList<>(documentation.values());
			} catch (Throwable exception) {
				//
			}
		}
		return Collections.emptyList();
	}

	private static String nullIfEmpty(String value) {
		return value == null ? "" : value; //$NON-NLS-1$
	}

	private static String getDefaultValue(Node node, String parameterName) {
		final var parameterNode = getSingleChild(node, parameterName);
		if (parameterNode instanceof Element cvalue) {
			return Strings.emptyIfNull(cvalue.getAttribute("default-value")); //$NON-NLS-1$
		}
		return null;
	}

	private static String getValue(Node node, String elementName)  {
		final var elementNode = getSingleChild(node, elementName);
		if (elementNode != null) {
			return elementNode.getTextContent();
		}
		return null;
	}

	private static boolean getBoolean(Node node, String elementName)  {
		final var value = getValue(node, elementName);
		if (!Strings.isEmpty(value)) {
			try {
				return Boolean.parseBoolean(value);
			} catch (Exception exception) {
				//
			}
		}
		return false;
	}

	private static Node getSingleChild(Node node, String elementName) {
		final var namedChild = findNamedChild(node, elementName);
		if (namedChild.isEmpty()) {
			return null;
		}
		if (namedChild.size() > 1) {
			return null;
		}
		return namedChild.get(0);
	}

	private static List<Node> findNamedChild(Node node, String elementName) {
		final var result = new ArrayList<Node>();
		final var childNodes = node.getChildNodes();
		for (var i = 0; i < childNodes.getLength(); ++i) {
			final var item = childNodes.item(i);
			if (elementName.equals(item.getNodeName())) {
				result.add(item);
			}
		}
		return result;
	}

	/** Run a static method on the given type.
	 *
	 * @param type is the type.
	 * @param methodName the name of the method.
	 * @return the configuration.
	 * @throws SecurityException if the method cannot be accessed.
	 * @throws NoSuchMethodException  is the method is undefined.
	 * @throws InvocationTargetException if the method cannot be invoked.
	 * @throws IllegalArgumentException if the arguments to the method are invalid.
	 * @throws IllegalAccessException if the access to the method is invalid.
	 * @since 0.12
	 */
	public static Object callStaticMethod(Class<?> type, String methodName) throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		final var method = type.getMethod(methodName);
		final var value = method.invoke(null);
		return value;
	}

}
