context('Single Select', () => {
  beforeEach(() => {
    cy.visit('./examples/SingleSelect/index.html')
  })

  // things that still need tests:
  // 1) when smart select is disabled
  // 2) pressing 'enter' to select (Cypress virtual event issue)
  // 3) if set, no results message

  describe('when select is enabled', () => {
    it('can be opened by clicking on it', () => {
      cy.get('#smart-select-component')
        .click()
        .should('have.class', 'elm-smart-select--enabled-opened')
    })
  
    it('can be closed by clicking outside of it', () => {
      cy.get('#smart-select-component')
        .click()
        .should('have.class', 'elm-smart-select--enabled-opened')
  
      cy.get('#form-submission-status').click()
  
      cy.get('#smart-select-component')
        .should('have.class', 'elm-smart-select--enabled-closed')
    })
  
    it('can filter options via search', () => {
      cy.get('#smart-select-component')
        .click()
      
      cy.get('#elm-smart-select--select-options-container')
        .children()
        .should('have.length', 9)

      cy.get('#smart-select-input')
        .type('3')

      cy.get('#elm-smart-select--select-options-container')
        .children()
        .should('have.length', 1)
    })

    it('can navigate options with \'Up\' and \'Down\' arrow keys', () => {
      cy.get('#smart-select-component')
        .click()

      cy.get('#elm-smart-select--select-options-container')
        .children()
        .first()
        .should('have.class', 'elm-smart-select--select-option-focused')

      cy.get('#smart-select-component')
        .type('{downarrow}')

      cy.get('#option-1')
        .should('have.class', 'elm-smart-select--select-option-focused')

      cy.get('#smart-select-component')
        .type('{uparrow}')

      cy.get('#option-0')
        .should('have.class', 'elm-smart-select--select-option-focused')
    })

    it('can be closed by pressing \'Escape\'', () => {
      cy.get('#smart-select-component')
        .click()
        .should('have.class', 'elm-smart-select--enabled-opened')
        .type('{esc}')
        .should('have.class', 'elm-smart-select--enabled-closed')
    })
  
    it('can select an option by clicking on it', () => {
      cy.get('#smart-select-component')
        .click()

      cy.get('#elm-smart-select--select-options-container')
        .children()
        .first()
        .click()

      cy.get('#smart-select-component')
        .should('have.class', 'elm-smart-select--enabled-closed')
        .and('have.text', 'product 1')
    })

    it('filters selected option from options', () => {
      cy.get('#smart-select-component')
        .click()

      cy.get('#elm-smart-select--select-options-container')
        .children()
        .first()
        .click()

      cy.get('#smart-select-component')
        .click()
      
      cy.get('#elm-smart-select--select-options-container')
        .children()
        .should('have.length', 8)
    })
  })
})
