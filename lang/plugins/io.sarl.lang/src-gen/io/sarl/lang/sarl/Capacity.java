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
 *   <li>{@link io.sarl.lang.sarl.Capacity#getSuperType <em>Super Type</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.Capacity#getActions <em>Actions</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getCapacity()
 * @model
 * @generated
 */
public interface Capacity extends AbstractElement
{
  /**
   * Returns the value of the '<em><b>Super Type</b></em>' reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Super Type</em>' reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Super Type</em>' reference.
   * @see #setSuperType(Capacity)
   * @see io.sarl.lang.sarl.SarlPackage#getCapacity_SuperType()
   * @model
   * @generated
   */
  Capacity getSuperType();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.Capacity#getSuperType <em>Super Type</em>}' reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Super Type</em>' reference.
   * @see #getSuperType()
   * @generated
   */
  void setSuperType(Capacity value);

  /**
   * Returns the value of the '<em><b>Actions</b></em>' containment reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.ActionSignature}.
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
  EList<ActionSignature> getActions();

} // Capacity
