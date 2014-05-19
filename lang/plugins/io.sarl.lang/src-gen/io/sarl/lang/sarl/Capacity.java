/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Capacity</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.Capacity#getActions <em>Actions</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getCapacity()
 * @model
 * @generated
 */
public interface Capacity extends InheritingElement
{
  /**
   * Returns the value of the '<em><b>Actions</b></em>' containment reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.Feature}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Actions</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Actions</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getCapacity_Actions()
   * @model containment="true"
   * @generated
   */
  EList<Feature> getActions();

} // Capacity
