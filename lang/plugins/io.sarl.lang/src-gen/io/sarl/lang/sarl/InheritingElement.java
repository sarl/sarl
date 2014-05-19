/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Inheriting Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.InheritingElement#getSuperTypes <em>Super Types</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getInheritingElement()
 * @model
 * @generated
 */
public interface InheritingElement extends TopElement, FeatureContainer
{
  /**
   * Returns the value of the '<em><b>Super Types</b></em>' reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.InheritingElement}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Super Types</em>' reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Super Types</em>' reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getInheritingElement_SuperTypes()
   * @model
   * @generated
   */
  EList<InheritingElement> getSuperTypes();

} // InheritingElement
