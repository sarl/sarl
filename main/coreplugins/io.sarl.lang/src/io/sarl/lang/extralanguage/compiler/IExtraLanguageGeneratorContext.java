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

package io.sarl.lang.extralanguage.compiler;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.actionprototype.IActionPrototypeContext;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;

/** The generator from SARL to the Python language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface IExtraLanguageGeneratorContext extends IGeneratorContext {

	/** Replies an identifier for the generation.
	 *
	 * @return the identifier.
	 */
	UUID getGenerationID();

	/** Replies the date of the generation.
	 *
	 * @return the date.
	 */
	Date getGenerationDate();

	/** Replies an identifier for the container of the generator's preferences.
	 *
	 * @return the identifier.
	 * @since 0.8
	 */
	String getPreferenceID();

	/** Replies the delegate.
	 *
	 * @return the delegate.
	 */
	IGeneratorContext getDelegate();

	/** Replies the root generator.
	 *
	 * @return the root generator.
	 */
	IRootGenerator getRootGenerator();

	/** Replies the resource from which the generation is done.
	 *
	 * @return the resource.
	 */
	Resource getResource();

	/** Replies the expected expression type in the context of a returnable expression
	 *
	 * <p>If the given generated expression should be the expression to be returned by a function,
	 * this function replies a type, otherwise {@code null}.
	 *
	 * @return the expected type.
	 */
	LightweightTypeReference getExpectedExpressionType();

	/** Set the expected expression type in the context of a returnable expression
	 *
	 * <p>If the given generated expression should be the expression to be returned by a function,
	 * this function replies a type, otherwise {@code null}.
	 *
	 * @param expectedType the expected type, or {@code null} if the expression is not expected to be returned by
	 *     a function.
	 * @return the value of the property before the change.
	 */
	LightweightTypeReference setExpectedExpressionType(LightweightTypeReference expectedType);

	/** Replies the file system access.
	 *
	 * @return the file system access.
	 */
	IFileSystemAccess2 getFileSystemAccess();

	/** Replies the context for the action prototype provider.
	 *
	 * @param provider the provider for creating the context if it was not created.
	 * @return the context.
	 * @since 0.10
	 */
	IActionPrototypeContext getActionPrototypeContext(IActionPrototypeProvider provider);

	/** Replies the stored data with the given identifier.
	 * If the data was not found, the default value is replied.
	 *
	 * @param <T> the type of the data.
	 * @param id the identifier.
	 * @param type the type of the data.
	 * @param defaultValue the default value.
	 * @return the data or the default value.
	 */
	<T> T getData(String id, Class<T> type, T defaultValue);

	/** Replies the stored data with the given identifier.
	 * If the data was not found, the default value is replied.
	 *
	 * @param <T> the type of the data.
	 * @param id the identifier.
	 * @param type the type of the data.
	 * @return the data or the default value.
	 */
	<T> T getData(String id, Class<T> type);

	/** Store data with the given identifier.
	 *
	 * @param id the identifier.
	 * @param value the value.
	 */
	void setData(String id, Object value);

	/** Clear all stored data.
	 */
	void clearData();

	/** Replies the stored data with the given identifier.
	 * If the data was not found, the default value is replied.
	 *
	 * @param <T> the type of the data.
	 * @param id the identifier.
	 * @return the data or the default value.
	 */
	<T> List<T> getListData(String id);

	/** Replies the stored data with the given identifier.
	 * If the data was not found, the default value is replied.
	 *
	 * @param <T> the type of the data.
	 * @param id the identifier.
	 * @return the data or the default value.
	 * @since 0.8
	 */
	<T> Set<T> getSetData(String id);

	/** Replies the stored data with the given identifier.
	 * If the data was not found, the default value is replied.
	 *
	 * @param <K> the type of the keys.
	 * @param <V> the type of the values.
	 * @param id the identifier.
	 * @return the data or the default value.
	 * @since 0.8
	 */
	<K, V> Map<K, V> getMapData(String id);

	/** Replies the stored data with the given identifier.
	 * If the data was not found, the default value is replied.
	 *
	 * @param <K> the type of the keys.
	 * @param <V> the type of the values.
	 * @param id the identifier.
	 * @return the data or the default value.
	 * @since 0.8
	 */
	<K, V> Map<K, List<V>> getMultimapData(String id);

	/** Replies the stored data with the given identifier.
	 * If the data was not found, the default value is replied.
	 *
	 * @param <K> the type of the keys.
	 * @param <V> the type of the values.
	 * @param id the identifier of the multimap.
	 * @param multimapKey the key within the multimap.
	 * @return the data or the default value.
	 * @since 0.8
	 */
	<K, V> List<V> getMultimapValues(String id, K multimapKey);

}
