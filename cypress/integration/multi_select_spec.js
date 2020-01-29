context('Single Select', () => {
  beforeEach(() => {
    cy.visit('./examples/MultiSelect/index.html')
  })

  // things that still need tests:
  // 1) when smart select is disabled
  // 2) pressing 'enter' to select (Cypress virtual event issue)
  // 3) if set, no results message for search
  // 4) if set, no options message when no more options exist (all are selected)

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
  
    it('can select multiple options by clicking on them', () => {
      cy.get('#smart-select-component')
        .click()

      cy.get('#elm-smart-select--select-options-container')
        .children()
        .first()
        .click()
        .click()

      cy.get('.elm-smart-select--multi-selected-container')
        .children()
        .first()
        .should('have.text', 'product 2 $5.00')
        .next()
        .should('have.text', 'product 1 $3.00')
    })

    it('can deselect options by clicking on them', () => {
      cy.get('#smart-select-component')
        .click()

      cy.get('#elm-smart-select--select-options-container')
        .children()
        .first()
        .click()
        .click()

      cy.get('.elm-smart-select--multi-selected-container')
        .children()
        // two selected options + search input
        .should('have.length', 3)

      cy.get('.elm-smart-select--multi-selected-container')
        .children()
        .first()
        .click()

      cy.get('.elm-smart-select--multi-selected-container')
        .children()
        // one selected option + search input
        .should('have.length', 2)
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
