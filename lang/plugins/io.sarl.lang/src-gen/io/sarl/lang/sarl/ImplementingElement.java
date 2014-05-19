/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Implementing Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.ImplementingElement#getImplementedTypes <em>Implemented Types</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getImplementingElement()
 * @model
 * @generated
 */
public interface ImplementingElement extends TopElement, InheritingElement
{
  /**
   * Returns the value of the '<em><b>Implemented Types</b></em>' reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.InheritingElement}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Implemented Types</em>' reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Implemented Types</em>' reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getImplementingElement_ImplementedTypes()
   * @model
   * @generated
   */
  EList<InheritingElement> getImplementedTypes();

} // ImplementingElement
