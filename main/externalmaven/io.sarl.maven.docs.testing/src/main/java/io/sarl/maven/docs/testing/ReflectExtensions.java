/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.maven.docs.testing;

import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.function.Function;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.google.common.collect.Iterables;
import org.eclipse.jdt.core.Flags;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SyntheticMember;
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

	private static Function<Method, String> defaultNameFormatter;

	private ReflectExtensions() {
		//
	}

	/** Change the default name formatter.
	 *
	 * @param formatter the default name formatter.
	 */
	public static void setDefaultNameFormatter(Function<Method, String> formatter) {
		defaultNameFormatter = formatter;
	}

	/** Replies the default name formatter.
	 *
	 * @return the default name formatter.
	 */
	@Pure
	public static Function<Method, String> getDefaultNameFormatter() {
		return defaultNameFormatter;
	}

	private static boolean isDeprecated(Method method) {
		return Flags.isDeprecated(method.getModifiers()) || method.getAnnotation(Deprecated.class) != null;
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
		final StringBuilder it = new StringBuilder();
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
		final List<String> lines = new LinkedList<>();
		for (final Class<?> type : types) {
			for (final Method method : type.getDeclaredMethods()) {
				if (Flags.isPublic(method.getModifiers()) && !Utils.isHiddenMember(method.getName())
						&& !isDeprecated(method) && !method.isSynthetic()
						&& method.getAnnotation(SyntheticMember.class) == null) {
					final StringBuilder line = new StringBuilder();
					if (indent) {
						line.append("\t"); //$NON-NLS-1$
					}
					Function<Method, String> nformatter = nameFormatter;
					if (nformatter == null) {
						nformatter = getDefaultNameFormatter();
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
						boolean first = true;
						int i = 1;
						for (final Parameter param : method.getParameters()) {
							if (first) {
								first = false;
							} else {
								line.append(", "); //$NON-NLS-1$
							}
							//it.append(param.getName());
							//it.append(" : "); //$NON-NLS-1$
							toType(line, param.getParameterizedType(), method.isVarArgs() && i == method.getParameterCount());
							final String defaultValue = extractDefaultValueString(param);
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
					line.append("\n"); //$NON-NLS-1$
					lines.add(line.toString());
				}
			}
		}
		lines.sort(null);
		for (final String line : lines) {
			it.append(line);
		}
	}

	private static String extractDefaultValueString(Parameter parameter) {
		final DefaultValue defaultValueAnnotation = parameter.getAnnotation(DefaultValue.class);
		if (defaultValueAnnotation == null) {
			return null;
		}
		final String fieldId = defaultValueAnnotation.value();
		if (!Strings.isEmpty(fieldId)) {
			final Class<?> container = parameter.getDeclaringExecutable().getDeclaringClass();
			if (container != null) {
				final int index = fieldId.indexOf('#');
				Class<?> target;
				final String fieldName;
				if (index > 0) {
					try {
						final Class<?> type = Class.forName(fieldId.substring(0, index), true, container.getClassLoader());
						target = type;
					} catch (Throwable exception) {
						target = container;
					}
					fieldName = Utils.createNameForHiddenDefaultValueAttribute(fieldId.substring(index + 1));
				} else {
					target = container;
					fieldName = Utils.createNameForHiddenDefaultValueAttribute(fieldId);
				}

				final Field field = Iterables.find(Arrays.asList(target.getDeclaredFields()),
						(it) -> Strings.equal(it.getName(), fieldName),
						null);
				if (field != null) {
					final SarlSourceCode sourceCodeAnnotation = parameter.getAnnotation(SarlSourceCode.class);
					if (sourceCodeAnnotation != null) {
						final String value = sourceCodeAnnotation.value();
						if (!Strings.isEmpty(fieldId)) {
							return value;
						}
					}
				}
			}
		}
		return null;
	}

	/** Extract the type name.
	 *
	 * @param it the output.
	 * @param otype the type to parse.
	 * @param isVarArg indicates if the type is used within a variadic parameter.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public static void toType(StringBuilder it, Type otype, boolean isVarArg) {
		final Type type;
		if (otype instanceof Class<?>) {
			type = isVarArg ? ((Class<?>) otype).getComponentType() : otype;
		} else {
			type = otype;
		}
		if (type instanceof Class<?>) {
			it.append(((Class<?>) type).getSimpleName());
		} else if (type instanceof ParameterizedType) {
			final ParameterizedType paramType = (ParameterizedType) type;
			final Type ownerType = paramType.getOwnerType();
			final boolean isForFunction = ownerType != null && Functions.class.getName().equals(ownerType.getTypeName());
			final boolean isForProcedure = ownerType != null && Procedures.class.getName().equals(ownerType.getTypeName());
			if (!isForFunction && !isForProcedure) {
				it.append(((Class<?>) paramType.getRawType()).getSimpleName());
				if (paramType.getActualTypeArguments().length > 0) {
					it.append("<"); //$NON-NLS-1$
					boolean first = true;
					for (final Type subtype : paramType.getActualTypeArguments()) {
						if (first) {
							first = false;
						} else {
							it.append(", "); //$NON-NLS-1$
						}
						final StringBuilder it2 = new StringBuilder();
						toType(it2, subtype, false);
						it.append(it2);
					}
					it.append(">"); //$NON-NLS-1$
				}
			} else {
				int nb = paramType.getActualTypeArguments().length;
				if (isForFunction) {
					--nb;
				}
				it.append("("); //$NON-NLS-1$
				for (int i = 0; i < nb; ++i) {
					final Type subtype = paramType.getActualTypeArguments()[i];
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
		} else if (type instanceof WildcardType) {
			final Type[] types = ((WildcardType) type).getUpperBounds();
			toType(it, types[0], false);
		} else if (type instanceof GenericArrayType) {
			toType(it, ((GenericArrayType) type).getGenericComponentType(), false);
			it.append("[]"); //$NON-NLS-1$
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
		final String resourceName = String.format(PLUGIN_HELP_PATH, mavenPluginGroupId, mavenPluginArtifactId);
		final URL url = classContext.getResource(resourceName);
		if (url != null) {
			try (InputStream is = url.openStream()) {
				final DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
				final DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
				final Document document = dBuilder.parse(is);
				final Node pluginNode = getSingleChild(document, "plugin"); //$NON-NLS-1$
				final Node mojosNode = getSingleChild(pluginNode, "mojos"); //$NON-NLS-1$

				final Map<String, List<String>> documentation = new TreeMap<>();

				for (final Node mojoNode : findNamedChild(mojosNode, "mojo")) { //$NON-NLS-1$
					final String mojoName = getValue(mojoNode, "goal"); //$NON-NLS-1$
					final Node parametersNode = getSingleChild(mojoNode, "parameters"); //$NON-NLS-1$
					final Node configurationNode = getSingleChild(mojoNode, "configuration"); //$NON-NLS-1$
					for (final Node parameterNode : findNamedChild(parametersNode, "parameter")) { //$NON-NLS-1$
						if (Strings.isEmpty(getValue(parameterNode, "deprecated")) //$NON-NLS-1$
								&& getBoolean(parameterNode, "editable")) { //$NON-NLS-1$
							final String name = getValue(parameterNode, "name"); //$NON-NLS-1$
							List<String> columns = documentation.get(name);
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
			} catch (Exception exception) {
				//
			}
		}
		return Collections.emptyList();
	}

	private static String nullIfEmpty(String value) {
		return value == null ? "" : value; //$NON-NLS-1$
	}

	private static String getDefaultValue(Node node, String parameterName) {
		final Node parameterNode = getSingleChild(node, parameterName);
		if (parameterNode instanceof Element) {
			return Strings.emptyIfNull(((Element) parameterNode).getAttribute("default-value")); //$NON-NLS-1$
		}
		return null;
	}

	private static String getValue(Node node, String elementName)  {
		final Node elementNode = getSingleChild(node, elementName);
		if (elementNode != null) {
			return elementNode.getTextContent();
		}
		return null;
	}

	private static boolean getBoolean(Node node, String elementName)  {
		final String value = getValue(node, elementName);
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
		final List<Node> namedChild = findNamedChild(node, elementName);
		if (namedChild.isEmpty()) {
			return null;
		}
		if (namedChild.size() > 1) {
			return null;
		}
		return namedChild.get(0);
	}

	private static List<Node> findNamedChild(Node node, String elementName) {
		final List<Node> result = new ArrayList<>();
		final NodeList childNodes = node.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); ++i) {
			final Node item = childNodes.item(i);
			if (elementName.equals(item.getNodeName())) {
				result.add(item);
			}
		}
		return result;
	}

}
